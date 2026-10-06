<#
Route 1: upscale a grainy 480p YouTube video to 1080p on your own Windows PC.

What it does, in order:
  1. Downloads portable tools into .\tools (yt-dlp, Deno, ffmpeg, Video2X 6.4.0 CLI,
     and the Real-ESRGAN general-v3 models from Video2X's master branch).
  2. Downloads the video with yt-dlp, keeping YouTube's original video and audio streams.
  3. Inspects it (size, aspect ratio, interlacing).
  4. Upscales a 10-second test clip with both general models and saves still frames to compare.
  5. Upscales the whole video with the model you pick.
  6. Scales to 1080 lines, encodes once with H.264, and copies the original audio in untouched.

Run it from PowerShell:
  powershell -ExecutionPolicy Bypass -File .\upscale-route1.ps1

Options:
  -Model strong|light   Skip the test question. strong = realesr-general-x4v3 (removes more grain),
                        light = realesr-general-wdn-x4v3 (weaker denoise, keeps more texture).
  -Device N             Vulkan GPU index for Video2X. The script lists your GPUs; on laptops with
                        two GPUs, pick the dedicated one.
  -TestStart 30         Start time in seconds of the 10-second test clip.
  -SkipTest             Go straight to the full upscale (needs -Model).

Needs: Windows 10/11 x64, a CPU with AVX2, a GPU with Vulkan support, and about 5 GB free space.
#>
param(
  [string]$Url = 'https://www.youtube.com/watch?v=K3VECwH0Drk',
  [string]$WorkDir = (Join-Path $HOME 'Videos\samanola-upscale'),
  [string]$Model = '',
  [int]$Device = 0,
  [double]$TestStart = 30,
  [switch]$SkipTest
)

$ErrorActionPreference = 'Stop'
$Model = $Model.Trim().ToLower()
if ($Model -and @('strong', 'light') -notcontains $Model) { throw '-Model must be strong or light.' }
$ProgressPreference = 'SilentlyContinue'   # makes Invoke-WebRequest much faster
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

function Step($text) { Write-Host ''; Write-Host "== $text" -ForegroundColor Cyan }
function Run($exe, [string[]]$argList) {
  & $exe @argList
  if ($LASTEXITCODE -ne 0) { throw "$([IO.Path]::GetFileName($exe)) failed with exit code $LASTEXITCODE" }
}
function Fetch($url, $dest) {
  if (-not (Test-Path $dest)) {
    Write-Host "Downloading $url"
    Invoke-WebRequest -Uri $url -OutFile $dest -UseBasicParsing
  }
}

New-Item -ItemType Directory -Force -Path $WorkDir | Out-Null
$Tools = Join-Path $WorkDir 'tools'
New-Item -ItemType Directory -Force -Path $Tools | Out-Null
Set-Location $WorkDir

# ---------------------------------------------------------------- 1. tools
Step '1/6  Getting tools'

$YtDlp = Join-Path $Tools 'yt-dlp.exe'
Fetch 'https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp.exe' $YtDlp

# yt-dlp needs a JavaScript runtime for full YouTube support; it finds deno.exe on PATH.
$DenoDir = Join-Path $Tools 'deno'
if (-not (Test-Path (Join-Path $DenoDir 'deno.exe'))) {
  $zip = Join-Path $Tools 'deno.zip'
  Fetch 'https://github.com/denoland/deno/releases/latest/download/deno-x86_64-pc-windows-msvc.zip' $zip
  Expand-Archive -Path $zip -DestinationPath $DenoDir -Force
}

$FfDir = Join-Path $Tools 'ffmpeg'
if (-not (Get-ChildItem -Path $FfDir -Recurse -Filter 'ffmpeg.exe' -ErrorAction SilentlyContinue)) {
  $zip = Join-Path $Tools 'ffmpeg.zip'
  Fetch 'https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-master-latest-win64-gpl.zip' $zip
  Expand-Archive -Path $zip -DestinationPath $FfDir -Force
}
$FfBin = (Get-ChildItem -Path $FfDir -Recurse -Filter 'ffmpeg.exe' | Select-Object -First 1).DirectoryName
$Ffmpeg = Join-Path $FfBin 'ffmpeg.exe'
$Ffprobe = Join-Path $FfBin 'ffprobe.exe'
$env:PATH = "$DenoDir;$FfBin;$env:PATH"

$V2xDir = Join-Path $Tools 'video2x'
$V2x = Join-Path $V2xDir 'video2x.exe'
if (-not (Test-Path $V2x)) {
  $zip = Join-Path $Tools 'video2x.zip'
  Fetch 'https://github.com/k4yt3x/video2x/releases/download/6.4.0/video2x-windows-amd64.zip' $zip
  Expand-Archive -Path $zip -DestinationPath $V2xDir -Force
}

# Video2X 6.4.0 does not ship the live-action "general" Real-ESRGAN models; they are only on its
# master branch. Its CLI accepts three model names, so this private copy puts the general models
# into two of those slots:
#   --realesrgan-model realesrgan-plus        -> realesr-general-x4v3      ("strong")
#   --realesrgan-model realesrgan-plus-anime  -> realesr-general-wdn-x4v3  ("light")
# The originals are kept as *.orig. Nothing outside .\tools is changed.
$ModelDir = Join-Path $V2xDir 'models\realesrgan'
$swap = @{ 'realesrgan-plus-x4' = 'realesr-generalv3-x4'; 'realesrgan-plus-anime-x4' = 'realesr-generalv3-wdn-x4' }
foreach ($slot in $swap.Keys) {
  foreach ($ext in 'param', 'bin') {
    $target = Join-Path $ModelDir "$slot.$ext"
    $marker = Join-Path $ModelDir "$slot.$ext.general"
    if (Test-Path $marker) { continue }
    if ((Test-Path $target) -and -not (Test-Path "$target.orig")) { Copy-Item $target "$target.orig" }
    # Pinned to a master-branch commit (7 Mar 2026) so the files can't change under you.
    $src = "https://raw.githubusercontent.com/k4yt3x/video2x/7db9c18d6278bbad9c3eda0e4e4ae210f9a688eb/models/realesrgan/$($swap[$slot]).$ext"
    Remove-Item $target -ErrorAction SilentlyContinue
    Fetch $src $target
    New-Item -ItemType File -Path $marker -Force | Out-Null
  }
}
$Slot = @{ strong = 'realesrgan-plus'; light = 'realesrgan-plus-anime' }

Write-Host ''
Write-Host 'Vulkan GPUs Video2X can see:'
Push-Location $V2xDir; & $V2x -l; Pop-Location
Write-Host "Using device $Device. If that is your integrated GPU, rerun with -Device <number of your dedicated GPU>."

# ---------------------------------------------------------------- 2. download
Step '2/6  Downloading the video (original streams, no re-encoding)'
$Source = Join-Path $WorkDir 'source.mkv'
if (-not (Test-Path $Source)) {
  Run $YtDlp @('-f', 'bv*+ba/b', '--merge-output-format', 'mkv', '--remux-video', 'mkv', '--ffmpeg-location', $FfBin,
               '-o', (Join-Path $WorkDir 'source.%(ext)s'), $Url)
}

# ---------------------------------------------------------------- 3. inspect
Step '3/6  Inspecting'
$probe = (& $Ffprobe -v error -select_streams v:0 -show_entries `
  'stream=width,height,sample_aspect_ratio,field_order,r_frame_rate:format=duration' -of json $Source | Out-String) | ConvertFrom-Json
$vs = $probe.streams[0]
$w = [int]$vs.width; $h = [int]$vs.height
$sarNum = 1; $sarDen = 1
if ($vs.sample_aspect_ratio -and $vs.sample_aspect_ratio -match '^(\d+):(\d+)$' -and [int]$Matches[1] -gt 0) {
  $sarNum = [int]$Matches[1]; $sarDen = [int]$Matches[2]
}
$dar = ($w * $sarNum) / ($h * $sarDen)
$TargetW = [int]([math]::Round(1080 * $dar / 2) * 2)
foreach ($std in 1440, 1920) { if ([math]::Abs($TargetW - $std) -le 4) { $TargetW = $std } }   # 854x480 -> 1920, not 1922
$interlaced = @('tt', 'bb', 'tb', 'bt') -contains "$($vs.field_order)"

# The container can say progressive while the picture still has combing baked in, so also run
# ffmpeg's interlace detector on the first 600 frames. (Redirecting stderr through Start-Process
# avoids a Windows PowerShell 5.1 quirk that turns redirected stderr into errors.)
$idetLog = Join-Path $WorkDir 'idet.log'
Start-Process -FilePath $Ffmpeg -NoNewWindow -Wait -RedirectStandardError $idetLog `
  -ArgumentList @('-hide_banner', '-i', "`"$Source`"", '-map', '0:v:0', '-vf', 'idet', '-frames:v', '600', '-f', 'null', '-')
$idet = Select-String -Path $idetLog -Pattern 'Multi frame detection: TFF:\s*(\d+)\s*BFF:\s*(\d+)\s*Progressive:\s*(\d+)' | Select-Object -Last 1
if ($idet) {
  $g = $idet.Matches[0].Groups
  $combed = [int]$g[1].Value + [int]$g[2].Value; $prog = [int]$g[3].Value
  Write-Host "Interlace check on 600 frames: $combed look interlaced, $prog look progressive."
  if ($combed -gt $prog) { $interlaced = $true }
}
$duration = [double]$probe.format.duration
Write-Host ("Source: {0}x{1}, display aspect {2:N3}, field order '{3}', {4} fps, {5:N0} s long" -f $w, $h, $dar, $vs.field_order, $vs.r_frame_rate, $duration)
Write-Host ("Target: {0}x1080" -f $TargetW)

# Deinterlace first if needed, otherwise the upscaler sharpens the combing into the picture.
$UpIn = $Source
if ($interlaced) {
  Write-Host 'Interlaced source: deinterlacing with bwdif into a lossless intermediate.'
  $UpIn = Join-Path $WorkDir 'source-deint.mkv'
  if (-not (Test-Path $UpIn)) {
    Run $Ffmpeg @('-hide_banner', '-y', '-i', $Source, '-map', '0:v:0', '-vf', 'bwdif=0', '-c:v', 'ffv1', $UpIn)
  }
}

function Upscale($in, $out, $which) {
  Push-Location $V2xDir   # Video2X looks for .\models relative to where it runs
  try {
    Run $V2x @('-i', $in, '-o', $out, '-d', "$Device", '-p', 'realesrgan', '-s', '4',
               '--realesrgan-model', $Slot[$which], '-c', 'libx264rgb', '-e', 'crf=12', '-e', 'preset=medium')
  } finally { Pop-Location }
}

# ---------------------------------------------------------------- 4. test
if (-not $SkipTest) {
  Step '4/6  10-second test with both models'
  $start = [math]::Min($TestStart, [math]::Max(0, $duration - 10))
  $Test = Join-Path $WorkDir 'test.mkv'
  Run $Ffmpeg @('-hide_banner', '-y', '-ss', "$start", '-i', $UpIn, '-t', '10', '-map', '0:v:0',
                '-c:v', 'libx264', '-crf', '10', '-preset', 'veryfast', $Test)
  $sw = [Diagnostics.Stopwatch]::StartNew()
  foreach ($which in 'strong', 'light') {
    $out = Join-Path $WorkDir "test-$which.mkv"
    if (-not (Test-Path $out)) { Upscale $Test $out $which }
  }
  $perClip = $sw.Elapsed.TotalSeconds / 2
  Write-Host ("Each 10-second test took about {0:N0} s, so the full video needs roughly {1:N0} min." -f $perClip, ($perClip * $duration / 10 / 60))

  # Still frames at 1080 lines from the same moment, for a side-by-side look.
  $scale = "scale=${TargetW}:1080:flags=lanczos,setsar=1"
  Run $Ffmpeg @('-hide_banner', '-y', '-loglevel', 'error', '-ss', '5', '-i', $Test, '-frames:v', '1', '-vf', $scale, (Join-Path $WorkDir 'compare-0-original.png'))
  Run $Ffmpeg @('-hide_banner', '-y', '-loglevel', 'error', '-ss', '5', '-i', (Join-Path $WorkDir 'test-strong.mkv'), '-frames:v', '1', '-vf', $scale, (Join-Path $WorkDir 'compare-1-strong.png'))
  Run $Ffmpeg @('-hide_banner', '-y', '-loglevel', 'error', '-ss', '5', '-i', (Join-Path $WorkDir 'test-light.mkv'), '-frames:v', '1', '-vf', $scale, (Join-Path $WorkDir 'compare-2-light.png'))
  Write-Host "Compare compare-0-original.png, compare-1-strong.png and compare-2-light.png in $WorkDir,"
  Write-Host 'and play test-strong.mkv and test-light.mkv full-screen. Pick the one where faces look like skin, not plastic.'
  Start-Process explorer.exe $WorkDir

  while (-not $Model) {
    $choice = (Read-Host 'Which model for the full video? Type strong or light').Trim().ToLower()
    if (@('strong', 'light') -contains $choice) { $Model = $choice }
  }
}
if (-not $Model) { throw 'Use -Model strong or -Model light together with -SkipTest.' }

# ---------------------------------------------------------------- 5. full upscale
Step "5/6  Upscaling the whole video with the $Model model"
$Up = Join-Path $WorkDir "upscaled-4x-$Model.mkv"
if (-not (Test-Path $Up)) { Upscale $UpIn $Up $Model }

# ---------------------------------------------------------------- 6. final encode + original audio
# Video2X can drop the last frame or two. Its output still starts at time 0, so the audio stays in
# sync; the final file just ends a fraction of a second earlier on the picture.
Step '6/6  Scaling to 1080, encoding, copying the original audio in'
$Final = Join-Path $WorkDir "final-1080p-$Model.mkv"
Run $Ffmpeg @('-hide_banner', '-y', '-i', $Up, '-i', $Source,
              '-map', '0:v:0', '-map', '1:a:0',
              '-vf', "scale=${TargetW}:1080:flags=lanczos,setsar=1",
              '-c:v', 'libx264', '-preset', 'slow', '-crf', '17', '-pix_fmt', 'yuv420p',
              '-c:a', 'copy', $Final)

Write-Host ''
Write-Host "Done: $Final" -ForegroundColor Green
Write-Host 'The audio track is the original, copied without re-encoding.'
