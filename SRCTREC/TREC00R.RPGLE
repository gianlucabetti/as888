000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110511     H* Trec00r *----------------------------------------------------*
000300110510     H* calcola importo di una voce
000400950517     H*--------------------------------------------------------------*
000500110510     Ftrec00d   CF   E             WORKSTN
000600110510     Fecevt01l  iF   E           k disk
000700110510     d trec00ds      e ds                  inz
000800110510     d kpjba         e ds
000900110510     D WLBDA8          DS
001000110510     D  G08DAT                 1      8  0
001100110510     D  G08INV                 9     16  0
001200110510     D  G08ERR                17     17
001300110510     D  G08TGI                18     22  0
001400110510     D datads          DS
001500110510     D  annov                  1      4  0
001600110510     D  mesev                  5      6  0
001700110510     D  giornov                7      8  0
001800110510     D  datav                  1      8  0
001900950517     C* Loop di gestione videata
002000050426     C     *ENTRY        PLIST
002100110510     C                   PARM                    kpjba
002200050426     c*
002300110114     C                   SETON                                        LR
002400110510     c                   clear                   datav
002500110510     c                   move      *year         annov
002600110228     c                   do        *hival
002700110510     C                   EXFMT     trec00
002800110510     c                   setoff                                       010203
002900110510     C                   IF        *inkc = '1'
003000050426     c                   leave
003100950517     C                   END
003200110510     c* anno
003300110510     c                   if        annov = 0
003400110510     c                   seton                                        02
003500110510     c                   iter
003600110510     c                   end
003700110510     c* mese
003800110510     c                   if        mesev = 0  and giornov <> 0
003900110510     c                   seton                                        03
004000110510     c                   iter
004100110510     c                   end
004200110510     c* voce
004300110510     c                   clear                   desvocev
004400110510     c                   if        vocev <> 0
004500110510     c     vocev         setgt     ecevt01l
004600110510     c     vocev         readpe    ecevt01l
004700110510     c                   if        %eof(ecevt01l)
004800110510     c                   seton                                        01
004900110510     c                   iter
005000110510     c                   else
005100110510     c                   movel     EVTVOD        desvocev
005200110510     c                   if        evttvo <> 'V'
005300110510     c                   seton                                        01
005400110510     c                   iter
005500110510     c                   end
005600110510     c                   end
005700110510     c                   else
005800110510     c                   seton                                        01
005900110510     c                   iter
006000050426     c                   end
006100110510     c* periodo
006200110510     c                   if        mesev <> 0
006300110510     C                   eval      com08 = datav
006400110510     c                   if        giornov = 0
006500110510     c                   move      01            com08             8 0
006600110510     c                   end
006700110510     C                   eval      G08inv  = com08
006800110510     C                   MOVEL     '3'           G08ERR
006900110510     C                   CALL      'XSRDA8'
007000110510     C                   PARM                    WLBDA8
007100110510     C                   if        G08err ='1'
007200110510     c                   seton                                        03
007300110510     c                   iter
007400110510     c                   end
007500110510     c                   end
007600140603     C                   IF        *inkf or *inkg
007700110510     c                   move      vocev         d00voce
007800110510     C                   eval      d00dta = datav
007900110510     c                   movel(p)  trec00ds      kpjbu
008000140603     c                   if        *inkf
008100110510     c                   call      'TRECVOC1C'
008200110510     c                   parm                    kpjba
008300140603     c                   else
008400140603     C                   MOVEL     'ECV1'        KCOAZ
008500140603     C                   CALL      'BCH10'
008600140603     C                   PARM                    KPJBA
008700140603     c                   end
008800110511     c                   leave
008900110510     C                   END
009000050426     C                   ENDdo
