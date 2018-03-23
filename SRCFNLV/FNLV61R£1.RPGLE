000100930527     H DECEDIT('0,') DATEDIT(*DMY.)
000200050826     H* FNLV61R *-----------------------------------------------------*
000300050826     H*     Reperimento data riferimento per pulizie di filiale
000400050826     H*
000500050826     h*     Questo pgm si è reso necessario per evitare che i lavori
000600050826     h*     di pulizie di filiale che partono a cavallo della mezzanotte
000700050826     h*     assumano date di pulizie differenti a parità di giorni di
000800050826     h*     pulizia.
000900050826     h*     Mediante questo pgm viene infatti determinata la "data di
001000050826     h*     riferimento" che passata al pgm di pulizia andrà a sostituire
001100050826     h*     udate per il calcolo delle date di pulizia.
001200050826     h*     La data di riferimento viene così determinata:
001300050826     h*    -UDATE >= (DataRif da tabella+gg da tabella) --> DATA RIF=UDATE
001400050826     h* ELSE
001500050826     h*    -UDATE <  (DataRif da tabella+gg da tabella) --> DATA RIF=DATA RI
001600050826     h*     F. DA TABELLA
001700050826     h*
001800050826     h*
001900050826     h*    LA TABELLA E' LA "VPO" rek "PULIZIE_FIL"
002000050829     H*    Se data riferimento su tabella = 0 verrà assunto udate e aggiorna
002100050826     h*    ta la tabella
002200050905     h*    Se numero gg da trascorrere = 0 verrà assunto 5
002300050829     h*    Allo stesso modo se tabella non trovata verrà assunto udate e
002400050905     h*    5 come giorni da trascorrere
002500930322     H*---------------------------------------------------------------*
002600930322     H* DATA BASE                                                     *
002700930322     H*---------------------------------------------------------------*
002800050826     Ftntbe01l  UF   E           K DISK
002900050714     D*
003000050826     D Dvpopulfil    E DS
003100050826     D WLBDAT          DS                  INZ
003200050826     D  G02DAT                 1      8  0
003300050826     D  G02INV                 9     16  0
003400050826     D  G02ERR                17     17
003500050826     D  G02TGI                18     22  0
003600050712     d
003700050826     d kcod            s                   Like(tbecod) inz('VPO')
003800050826     d kkey            s                   Like(tbeke1) inz('PULIZIE_FIL    ')
003900050826     d §vpopfdr_x      s                   Like(§vpopfdr)
004000050826     d dataiso         s               d   datfmt(*iso)
004100930322     C*---------------------------------------------------------------*
004200050826     C     KTbe          KLIST
004300050826     C                   KFLD                    kcod
004400050826     C                   KFLD                    kkey
004500050826     C
004600050826     C     *ENTRY        PLIST
004700050826     C                   PARM                    O61DRF            8 0
004800090415     C                   PARM                    i61rep            1
004900970902     C**
005000050826     C                   CLEAR                   O61DRF
005100090415     c
005200090415     c** Se passato 2° parametro vedo se è solo in reperimento
005300090415     c                   if        %parms=2
005400090415     c                   movel     i61rep        solorep           1
005500090415     c                   else
005600090415     c                   clear                   soloREP
005700090415     c                   endif
005800050826     c*
005900050826     C                   TIME                    W0140            14 0
006000050826     C                   CLEAR                   WLBDAT
006100050826      * UDATE IN AAAAMMGG
006200050826     C                   MOVE      W0140         G02DAT
006300050826     C                   CALL      'XSRDA8'
006400050826     C                   PARM                    WLBDAT
006500050826     C                   Z-ADD     G02INV        WDateu            8 0
006600050826     c* chaino record di tabella contenente l'ultima data di riferimento
006700050826     c* utilizzata e il numero di giorni da trascorrere per il calcolo
006800050826     c* della nuova data di riferimento
006900050826     c                   clear                   dvpopulfil
007000050826     c     ktbe          chain(N)  tntbe01l
007100050826     c                   if        %found(tntbe01l)
007200050826     c                   movel     tbeuni        dvpopulfil
007300050826     c                   endif
007400090415     c
007500090415     c* se solo reperimento valorizzo il campo ed esco
007600090415     c                   if        soloREP='S'
007700090415     c
007800090415     c                   if        §vpopfdr = 0
007900090415     c                   z-add     wdateu        o61drf
008000090415     c                   else
008100090415     c                   z-add     §vpopfdr      o61drf
008200090415     c                   endif
008300090415     c                   goto      fine
008400090415     c
008500090415     c                   endif
008600090415     c
008700050826     c* se la data di riferimento in tabella è 0 assumo udate come data di
008800050826     c* riferimento e aggiorno la tabella con udate
008900050826     c                   if        §vpopfdr = 0
009000050826     c                   z-add     wdateu        o61drf
009100050826     c                   exsr      agg_tbe
009200050826     c                   else
009300050826     c* aggiungo alla data di rif. il numero dei giorni da trascorrere
009400050826     c* e confronto la data ottenuta con udate
009500050826     c                   move      §vpopfdr      dataiso
009600050829     c                   if        §vpopfgg = 0
009700050905     c                   z-add     5             §vpopfgg
009800050829     c                   endif
009900050826     c     dataiso       adddur    §vpopfgg:*d   dataiso
010000050826     c                   move      dataiso       §vpopfdr_x
010100050826     c* se la data del lavoro è >= alla ipotetica prossima data di riferimen
010200050826     c* to assumo come nuova data di riferimento la data del lavoro e
010300050826     c* aggiorno la tabella
010400050826     c                   if        wdateu >= §vpopfdr_x
010500050826     c                   z-add     wdateu        o61drf
010600050826     c                   exsr      agg_tbe
010700050826     c                   else
010800050826     c* se la data del lavoro è < alla ipotetica prossima data di riferiment
010900050826     c* mantengo come data di riferimento quella della tabella
011000050826     c                   z-add     §vpopfdr      o61drf
011100050826     c                   endif
011200050826     c                   endif
011300050826     c
011400050826     C*
011500090415     c     fine          tag
011600090415     c
011700970902     C                   SETON                                        LR
011800050826     C**********************************************************************
011900050826     c* Aggiornamento data riferimento su tabella "VPO" rek "PULIZIE_FIL"
012000050826     C**********************************************************************
012100050826     c     agg_tbe       begsr
012200050826     c     ktbe          chain     tntbe01l
012300050826     c                   if        %found(tntbe01l)
012400050826     c                   movel     tbeuni        dvpopulfil
012500050826     c                   z-add     o61drf        §vpopfdr
012600050829     c                   if        §vpopfgg = 0
012700050905     c                   z-add     5             §vpopfgg
012800050829     c                   endif
012900050826     c                   movel     dvpopulfil    tbeuni
013000050826     c                   update    tntbe000
013100050826     c                   endif
013200050826     c                   endsr
