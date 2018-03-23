000100161114     h decedit('0,') datedit(*ymd.)
000200161114      **
000300130724     c                   clear                   dal               8 0
000400130722     c                   clear                   al                8 0
000500161114     c                   clear                   trasmessi         1
000600130722     C/EXEC SQL
000700130724     c+   select MIN(FTTDDC), MAX(fTTDDC) INTO :DAL, :AL
000800161114     c+   from fIFTT00f WHERE FTTTSR = ' '  and FTTFTR = ' '
000900130722     C/END-EXEC
001000161114      **
001100161114      ** pgm per sistemare le differenze fra FCE e FTT (di FTT trasemssi e NON)
001200130724     c                   if        dal <> 0 and al <> 0
001300161114     c                   call      'FICN43R2'
001400161114     c                   parm                    trasmessi         1
001500161114     c                   parm                    dal               8 0
001600161114     c                   parm                    al                8 0
001700130724     c                   end
001800161114      **
001900130722     c                   seton                                        lr
