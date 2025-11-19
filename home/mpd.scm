(define-module (home mpd)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module ((gnu packages mpd) #:select (mpdscribble
                                             mpdris2))
  #:use-module ((gnu packages audio) #:select (cava))

  #:use-module ((incognita packages rust-apps) #:select (rmpc))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services audio)

  #:use-module (home audio)

  #:use-module ((home theme) #:prefix theme:))

(define-public mpdris2-fixed
  (package
    (inherit mpdris2)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eonpatapon/mpDris2")
                    (commit "e40238ba1d3c7a81d34e5f96b150e40b5b29d4af")))
              (sha256 (base32 "1jnkr3xxsiq1mwnkviyr27ggr6ib50qkihpm9jsg1a253ck9w0lr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mpdris2)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'wrap-program
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((python-path (getenv "GUIX_PYTHONPATH"))
                      (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                  (wrap-program (string-append #$output "/bin/mpDris2")
                    `("GUIX_PYTHONPATH" ":" prefix (,python-path))
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))))))))))

(define mpdris2-config (mixed-text-file "mpdris2-config" "\
[Library]
music_dir = ~/Music
"))

(define mpdscribble-config (mixed-text-file "mpdscribble-config" "\
[last.fm]
url = https://post.audioscrobbler.com/
username = ToxicSalt
password = " (call-with-input-file "/home/tobias/irthir/irthos/last.fm" get-line) "
[listenbrainz]
url = http://proxy.listenbrainz.org/
username = nattaktiv
password = " (call-with-input-file "/home/tobias/irthir/irthos/listenbrainz" get-line) "
"))

(define ncmpcpp-config (mixed-text-file "ncmpcpp-config" "
[Library]
music_dir = ~/Music
"))

(define rmpc-config (mixed-text-file "rmpc-config" "\
#![enable(implicit_some)]
#![enable(unwrap_newtypes)]
#![enable(unwrap_variant_newtypes)]
(
    volume_step: 2,
    enable_config_hot_reload: false,
    theme: Some(\"theme\"),
    tabs: [
        (
            name: \"Queue\",
            pane: Pane(Queue),
        ),
        (
            name: \"Lyrics\",
            pane: Pane(Lyrics),
        ),
        (
            name: \"Directories\",
            pane: Pane(Directories),
        ),
        (
            name: \"Artists\",
            pane: Pane(Artists),
        ),
        (
            name: \"Album Artists\",
            pane: Pane(AlbumArtists),
        ),
        (
            name: \"Albums\",
            pane: Pane(Albums),
        ),
        (
            name: \"Playlists\",
            pane: Pane(Playlists),
        ),
        (
            name: \"Search\",
            pane: Pane(Search),
        ),
    ],
)
"))

(define rmpc-theme (mixed-text-file "rmpc-theme" "\
#![enable(implicit_some)]
#![enable(unwrap_newtypes)]
#![enable(unwrap_variant_newtypes)]
(
    default_album_art_path: None,
    show_song_table_header: true,
    draw_borders: false,
    format_tag_separator: \" | \",
    browser_column_widths: [20, 38, 42],
    background_color: None,
    text_color: None,
    header_background_color: None,
    modal_background_color: None,
    modal_backdrop: false,
    preview_label_style: (fg: \"yellow\"),
    preview_metadata_group_style: (fg: \"yellow\", modifiers: \"Bold\"),
    tab_bar: (
        enabled: true,
        active_style: (fg: \"" theme:bg "\", bg: \"" theme:fg "\", modifiers: \"Bold\"),
        inactive_style: (),
    ),
    highlighted_item_style: (fg: \"" theme:highlight "\", modifiers: \"Bold\"),
    current_item_style: (fg: \"" theme:bg "\", bg: \"" theme:fg "\", modifiers: \"Bold\"),
    borders_style: (fg: \"" theme:accent "\"),
    highlight_border_style: (fg: \"" theme:highlight "\"),
    symbols: (
        song: \"S\",
        dir: \"D\",
        playlist: \"P\",
        marker: \"M\",
        ellipsis: \"...\",
        song_style: None,
        dir_style: None,
        playlist_style: None,
    ),
    level_styles: (
        info: (fg: \"blue\", bg: \"black\"),
        warn: (fg: \"yellow\", bg: \"black\"),
        error: (fg: \"red\", bg: \"black\"),
        debug: (fg: \"light_green\", bg: \"black\"),
        trace: (fg: \"magenta\", bg: \"black\"),
    ),
    progress_bar: (
        symbols: [\"⣿\", \"⣿\", \"⣿\", \"⢕\", \"⢕\"],
        track_style: (fg: \"" theme:accent "\"),
        elapsed_style: (fg: \"" theme:fg "\"),
        thumb_style: (fg: \"" theme:fg "\"),
    ),
    scrollbar: (
        symbols: [\"⢕\", \"⣿\", \"⣠\", \"⠙\"],
        track_style: (fg: \"" theme:accent "\"),
        ends_style: (fg: \"" theme:fg "\"),
        thumb_style: (fg: \"" theme:fg "\"),
    ),
    song_table_format: [
        (
            prop: (kind: Property(Artist),
                default: (kind: Text(\"Unknown\"))
            ),
            width: \"20%\",
        ),
        (
            prop: (kind: Property(Title),
                default: (kind: Text(\"Unknown\"))
            ),
            width: \"35%\",
        ),
        (
            prop: (kind: Property(Album), style: (fg: \"white\"),
                default: (kind: Text(\"Unknown Album\"), style: (fg: \"white\"))
            ),
            width: \"30%\",
        ),
        (
            prop: (kind: Property(Duration),
                default: (kind: Text(\"-\"))
            ),
            width: \"15%\",
            alignment: Right,
        ),
    ],
    components: {},
    layout: Split(
        direction: Vertical,
        panes: [
            (
                size: \"8\",
                pane: Split(
                    direction: Horizontal,
                    panes: [
                        (
                            size: \"21\",
                            pane: Pane(AlbumArt),
                        ),
                        (
                            size: \"100%\",
                            pane: Split(
                                direction: Vertical,
                                panes: [
                                    (
                                        size: \"5\",
                                        pane: Pane(Header),
                                    ),
                                    (
                                        size: \"1\",
                                        pane: Pane(ProgressBar),
                                    ),
                                    (
                                        size: \"3\",
                                        pane: Pane(Tabs),
                                    ),
                                ]
                            )
                        ),
                    ]
                ),
            ),
            (
                size: \"100%\",
                pane: Pane(TabContent),
            ),
        ],
    ),
    header: (
        rows: [
            (
                left: [
                    (kind: Text(\"[\"), style: (fg: \"" theme:highlight "\", modifiers: \"Bold\")),
                    (kind: Property(Status(StateV2(playing_label: \"Playing\", paused_label: \"Paused\", stopped_label: \"Stopped\"))), style: (fg: \"" theme:highlight "\", modifiers: \"Bold\")),
                    (kind: Text(\"]\"), style: (fg: \"" theme:highlight "\", modifiers: \"Bold\"))
                ],
                center: [
                    (kind: Property(Song(Title)), style: (modifiers: \"Bold\"),
                        default: (kind: Text(\"No Song\"), style: (modifiers: \"Bold\"))
                    )
                ],
                right: [
                    (kind: Property(Widget(ScanStatus)), style: (fg: \"" theme:highlight "\")),
                    (kind: Property(Widget(Volume)), style: (fg: \"" theme:highlight "\"))
                ]
            ),
            (
                left: [
                    (kind: Property(Status(Elapsed))),
                    (kind: Text(\" / \")),
                    (kind: Property(Status(Duration))),
                    (kind: Text(\" (\")),
                    (kind: Property(Status(Bitrate))),
                    (kind: Text(\" kbps)\"))
                ],
                center: [
                    (kind: Property(Song(Artist)), style: (fg: \"" theme:highlight "\", modifiers: \"Bold\"),
                        default: (kind: Text(\"Unknown\"), style: (fg: \"" theme:highlight "\", modifiers: \"Bold\"))
                    ),
                    (kind: Text(\" - \")),
                    (kind: Property(Song(Album)),
                        default: (kind: Text(\"Unknown Album\"))
                    )
                ],
                right: [
                    (
                        kind: Property(Widget(States(
                            active_style: (fg: \"white\", modifiers: \"Bold\"),
                            separator_style: (fg: \"white\")))
                        ),
                        style: (fg: \"" theme:accent "\")
                    ),
                ]
            ),
        ],
    ),
    browser_song_format: [
        (
            kind: Group([
                (kind: Property(Track)),
                (kind: Text(\" \")),
            ])
        ),
        (
            kind: Group([
                (kind: Property(Artist)),
                (kind: Text(\" - \")),
                (kind: Property(Title)),
            ]),
            default: (kind: Property(Filename))
        ),
    ],
    lyrics: (
        timestamp: false
    )
)
"))

(define-public services
  (list
   (service home-mpd-service-type
            (home-mpd-configuration
             (pipewire-output? #t)))
   (simple-service 'mpd-xdg-config home-xdg-configuration-files-service-type
                   `(("mpDris2/mpDris2.conf" ,mpdris2-config)
                     ("mpdscribble/mpdscribble.conf" ,mpdscribble-config)
                     ("rmpc/config.ron" ,rmpc-config)
                     ("rmpc/themes/theme.ron" ,rmpc-theme)))))

(define-public packages
  (list mpdscribble
        mpdris2-fixed
        rmpc))
