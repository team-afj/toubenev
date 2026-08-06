let () = Logs.set_reporter (Logs_browser.console_reporter ())
let () = Logs.set_level ~all:true (Some Debug)

include (val Logs.src_log (Logs.Src.create "TBN"))
