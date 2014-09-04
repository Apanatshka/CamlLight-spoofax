let rec even e = if e = 0 then e = 0 else odd (e - 1)
and odd o = if o = 1 then o = 1 else even (o - 1)
;;