(define-module (home qutebrowser qutebrowser)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module ((gnu packages web-browsers) #:select (qutebrowser))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((home theme) #:prefix theme:))

(define user-stylesheet (mixed-text-file "user-stylesheet" "
body *, body *::before, body *::after {
  transition: 0.01ms !important;
  transition-delay: 0ms !important;
  animation-delay: 0ms !important;
  animation-duration: 0ms !important;

  border-radius: 0px !important;
  border-color: " theme:accent " !important;
}

body {
  background-color: " theme:bg " !important;
  color: " theme:fg " !important;
}

html {
  background-color: " theme:bg " !important;
}
"))

(define ffz-userscript
  (origin
    (uri "https://cdn.frankerfacez.com/static/ffz_injector.user.js")
    (method url-fetch)
    (sha256 (base32 "0vl038x7mm98ghrirr5zcdv24zmhfaj7mrygcm07qs6dri99yjsl"))))

(define screenshare-with-audio-userscript
  (origin
    (uri "https://update.greasyfork.org/scripts/436013/Screenshare_with_Audio.user.js")
    (method url-fetch)
    (sha256 (base32 "1szp9c0xvpmkam1b39d2rdj46w5jmf3zdhb7c993xpjmxryc8pqv"))))

(define qb-discord-desktop-entry (mixed-text-file "qb-discord-desktop-entry" "
[Desktop Entry]
Name=Discord (QB)
Exec=" (if (string= (gethostname) "okarthel")
           (replace-mesa qutebrowser)
           qutebrowser) "/bin/qutebrowser --target window -- https://canary.discord.com/channels/@me
Type=Application
Terminal=false"))

(define qutebrowser-config (mixed-text-file "qutebrowser-config" "
from qutebrowser.api import interceptor
from PyQt6.QtCore import QUrl
import operator

config.load_autoconfig()

c.completion.web_history.exclude = ['duckduckgo.com', 'twitter.com', '*.reddit.com', '*.discord.com']

c.content.notifications.enabled = False
c.content.notifications.show_origin = False

with config.pattern('*.discord.com') as p:
    p.content.notifications.enabled = True
    p.content.desktop_capture = True
    p.content.media.audio_capture = True
    p.content.media.video_capture = True
    p.content.media.audio_video_capture = True
    p.content.autoplay = True

c.content.blocking.enabled = True
c.content.blocking.method = 'adblock'
c.content.blocking.adblock.lists = [
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/filters.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/annoyances-cookies.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/annoyances-others.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/badware.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/privacy.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/resource-abuse.txt',
    'https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/unbreak.txt',
    'https://secure.fanboy.co.nz/r/fanboy-ultimate.txt',
    'https://secure.fanboy.co.nz/fanboy-social.txt',
    'https://secure.fanboy.co.nz/fanboy-antifonts.txt',
    'https://secure.fanboy.co.nz/fanboy-antifacebook.txt',
    'https://secure.fanboy.co.nz/enhancedstats.txt',
    'https://raw.githubusercontent.com/yokoffing/filterlists/main/block_third_party_fonts.txt',
    'https://raw.githubusercontent.com/yokoffing/filterlists/main/annoyance_list.txt',
    'https://raw.githubusercontent.com/yokoffing/filterlists/main/click2load.txt',
    'https://raw.githubusercontent.com/DandelionSprout/adfilt/master/Dandelion%20Sprout%27s%20Anti-Malware%20List.txt',
    'https://raw.githubusercontent.com/DandelionSprout/adfilt/master/LegitimateURLShortener.txt',
    'https://raw.githubusercontent.com/DandelionSprout/adfilt/master/BrowseWebsitesWithoutLoggingIn.txt'
]

c.qt.args = [
    'ignore-gpu-blocklist',
    'enable-zero-copy',
    'enable-gpu-rasterization',
    'enable-accelerated-2d-canvas',
    'enable-accelerated-video-decode',
    'enable-drdc',
    'enable-raw-draw',
    'use-gl=desktop',
    'enable-features=WebRTCPipeWireCapturer'
]

c.qt.workarounds.disable_accelerated_2d_canvas = 'never'
c.qt.chromium.experimental_web_platform_features = 'always'

c.content.prefers_reduced_motion = True
c.content.autoplay = False
c.input.media_keys = False
c.content.pdfjs = True

o = operator.methodcaller
s = 'setHost'
m = {
    \"reddit.com\" : o(s, 'old.reddit.com'),
    \"www.reddit.com\" : o(s, 'old.reddit.com'),

    \"youtu.be\" : o(s, 'piped.garudalinux.org'),
    \"youtube.com\" : o(s, 'piped.garudalinux.org'),
    \"www.youtube.com\" : o(s, 'piped.garudalinux.org')
}

def rewrite(info: interceptor.Request):
    if (info.resource_type != interceptor.ResourceType.main_frame or
            info.request_url.scheme() in {\"data\", \"blob\"}):
        return
    url = info.request_url
    redir = m.get(url.host())
    if redir is not None and redir(url) is not False:
        info.redirect(url)
interceptor.register(rewrite)

c.window.title_format = '{current_url}'

c.colors.webpage.preferred_color_scheme = 'dark'
c.colors.webpage.bg = 'black'
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'never'
c.colors.webpage.darkmode.policy.page = 'always'
c.content.user_stylesheets = '" user-stylesheet "'

c.downloads.position = 'bottom'

c.fonts.default_family = '" theme:font-mono "'
c.fonts.default_size = '" theme:font-size "pt'

c.hints.border = '1px solid " theme:bg "'
c.hints.radius = 0
c.colors.hints.bg = '" theme:fg "'
c.colors.hints.fg = '" theme:bg "'
c.colors.hints.match.fg = '" theme:highlight "'

c.keyhint.delay = 0
c.keyhint.radius = 0
c.colors.keyhint.bg = '" theme:bg "'
c.colors.keyhint.fg = '" theme:fg "'
c.colors.keyhint.suffix.fg = '" theme:highlight "'

c.prompt.radius = 0

c.completion.scrollbar.width = 0
c.colors.completion.fg = '" theme:fg "'
c.colors.completion.category.bg = '" theme:accent "'
c.colors.completion.category.fg = '" theme:fg "'
c.colors.completion.category.border.top = '" theme:fg "'
c.colors.completion.category.border.bottom = '" theme:fg "'
c.colors.completion.even.bg = '" theme:bg "'
c.colors.completion.odd.bg = '" theme:bg "'
c.colors.completion.item.selected.fg = '" theme:fg "'
c.colors.completion.item.selected.bg = '" theme:accent "'
c.colors.completion.item.selected.border.top = '" theme:accent "'
c.colors.completion.item.selected.border.bottom = '" theme:accent "'
c.colors.completion.match.fg = '" theme:highlight "'
c.colors.completion.scrollbar.bg = '" theme:bg "'
c.colors.completion.scrollbar.fg = '" theme:fg "'

c.tabs.background = True
c.tabs.show = 'multiple'
c.tabs.position = 'top'
c.tabs.indicator.width = 0
c.tabs.padding = {'bottom': 2, 'left': 5, 'right': 5, 'top': 2}
c.tabs.title.format = '{audio}{current_title} {private}' # {index}:
c.colors.tabs.bar.bg = '" theme:bg "'
c.colors.tabs.even.bg = '" theme:bg "'
c.colors.tabs.even.fg = '" theme:fg "'
c.colors.tabs.odd.bg = '" theme:bg "'
c.colors.tabs.odd.fg = '" theme:fg "'
c.colors.tabs.selected.even.bg = '" theme:fg "'
c.colors.tabs.selected.even.fg = '" theme:bg "'
c.colors.tabs.selected.odd.bg = '" theme:fg "'
c.colors.tabs.selected.odd.fg = '" theme:bg "'

c.statusbar.padding = {'bottom': 2, 'left': 1, 'right': 1, 'top': 2}
c.statusbar.show = 'in-mode'
c.colors.statusbar.normal.bg = '" theme:bg "'
c.colors.statusbar.normal.fg = '" theme:fg "'
c.colors.statusbar.insert.bg = '" theme:accent "'
c.colors.statusbar.insert.fg = '" theme:fg "'
c.colors.statusbar.passthrough.bg = '" theme:accent "'
c.colors.statusbar.passthrough.fg = '" theme:fg "'
c.colors.statusbar.url.hover.fg = '" theme:accent "'

config.bind('gk', 'scroll-to-perc 0')
config.bind('gj', 'scroll-to-perc 100')

config.bind(',m', 'hint links spawn mpv {hint-url} --ytdl-raw-options=cookies-from-browser=chromium:~/.local/share/qutebrowser')
config.bind(',M', 'spawn mpv {url} --ytdl-raw-options=cookies-from-browser=chromium:~/.local/share/qutebrowser')
config.bind(',t', 'hint links spawn webtorrent --mpv {hint-url}')

config.bind('<Ctrl-z>', 'config-cycle tabs.position right top')

config.bind('+', 'zoom-in')
config.bind('-', 'zoom-out')
"))

(define-public packages
  (list
   (if (or (string= (gethostname) "okarthel")
           (string= (gethostname) "austrat"))
       (replace-mesa qutebrowser)
       qutebrowser)))

(define-public services
  (list
   (simple-service 'qutebrowser-desktop-entries
                   home-xdg-data-files-service-type
                   `(("applications/qb-discord.desktop" ,qb-discord-desktop-entry)))
   (simple-service 'qutebrowser-config
                   home-xdg-configuration-files-service-type
                   `(("qutebrowser/config.py" ,qutebrowser-config)
                     ("qutebrowser/greasemonkey/ffz_injector.user.js" ,ffz-userscript)
                     ("qutebrowser/greasemonkey/Screenshare_With_audio.user.js" ,screenshare-with-audio-userscript)))))
