# trmote

Useful programs built on top of
[transmission.scm](https://github.com/siiky/transmission.scm) to
interact with/control a Transmission daemon.

## `transmission-filter`

Filter and list torrents sorted by descending upload ratio. Useful to find the
most seeded torrents when you need to free up some disk space, or want to
choose the next thing to watch/read before deleting.

## `transmission-labels`

Add, get, remove, and set labels of the given torrent(s). Not as useful now
that `transmission-remote` shows a torrent's labels (`-i`/`--info`), and has an
option to change a torrent's labels (`-L`/`--labels`), but still easier to use.

## `transmission-update-seed-priority`

Update the priority (low, normal, high) of torrents depending on upload ratio.
The default values were chosen to my liking, to make my life easier, so I just
have something like this set on my crontab to run hourly:

```sh
transmission-update-seed-priority -ne --skip-label TRMOTE-NO_AUTO_PRIORITY
```

## `transmission-verify`

Verify torrents, but trying to maximize "uptime" (upload/download). Simply asks
Transmission to verify a torrent, waits for it to finish (with active polling),
and asks to verify the next one. Different sorting parameters and combinations
may be used to customize the order of the torrents to be verified. As of now,
the parameters are: id (ascending); latest activity (descending, most recent
first); priority (descending); progress percentage (descending); queue position
(ascending); upload ratio (ascending); status (verifying, waiting to verify,
seeding, downloading, waiting to seed, waiting to download, stopped); total
size (ascending); progress percentage times total size (i.e. downloaded size,
ascending). New parameters can be added easily (requires recompiling). The
default sorting parameters are (as of now) status, priority, latest activity,
upload ratio, progress percentage times total size. For more details, check the
NOTEs comments.

On my crontab I have something like this set to run monthly:

```sh
transmission-verify -ne --daemon --logfile /path/to/transmission-verify.log
```
