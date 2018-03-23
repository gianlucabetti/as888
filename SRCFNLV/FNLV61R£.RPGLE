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
003700100618     d dvpopuladd    e ds
003800100618     d parmpuladd    e ds                  extname(dvpopuladd) prefix(P)
003900050826     d kcod            s                   Like(tbecod) inz('VPO')
004000050826     d kkey            s                   Like(tbeke1) inz('PULIZIE_FIL    ')
004100050826     d §vpopfdr_x      s                   Like(§vpopfdr)
004200050826     d dataiso         s               d   datfmt(*iso)
004300930322     C*---------------------------------------------------------------*
004400050826     C     KTbe          KLIST
004500050826     C                   KFLD                    kcod
004600050826     C                   KFLD                    kkey
004700100629     C*---------------------------------------------------------------*
004800100629
004900050826     C     *ENTRY        PLIST
005000050826     C                   PARM                    O61DRF            8 0
005100090415     C                   PARM                    i61rep            1
005200100618     C                   PARM                    parmpuladd
005300970902     C**
005400050826     C                   CLEAR                   O61DRF
005500090415     c
005600090415     c** Se passato 2° parametro vedo se è solo in reperimento
005700100618     c                   if        %parms>=2
005800090415     c                   movel     i61rep        solorep           1
005900090415     c                   else
006000090415     c                   clear                   soloREP
006100090415     c                   endif
006200050826     c*
006300050826     C                   TIME                    W0140            14 0
006400050826     C                   CLEAR                   WLBDAT
006500050826      * UDATE IN AAAAMMGG
006600050826     C                   MOVE      W0140         G02DAT
006700050826     C                   CALL      'XSRDA8'
006800050826     C                   PARM                    WLBDAT
006900050826     C                   Z-ADD     G02INV        WDateu            8 0
007000050826     c* chaino record di tabella contenente l'ultima data di riferimento
007100050826     c* utilizzata e il numero di giorni da trascorrere per il calcolo
007200050826     c* della nuova data di riferimento
007300050826     c                   clear                   dvpopulfil
007400050826     c     ktbe          chain(N)  tntbe01l
007500050826     c                   if        %found(tntbe01l)
007600050826     c                   movel     tbeuni        dvpopulfil
007700050826     c                   endif
007800090415     c
007900090415     c* se solo reperimento valorizzo il campo ed esco
008000090415     c                   if        soloREP='S'
008100090415     c
008200090415     c                   if        §vpopfdr = 0
008300090415     c                   z-add     wdateu        o61drf
008400090415     c                   else
008500090415     c                   z-add     §vpopfdr      o61drf
008600090415     c                   endif
008700090415     c                   goto      fine
008800090415     c
008900090415     c                   endif
009000090415     c
009100050826     c* se la data di riferimento in tabella è 0 assumo udate come data di
009200050826     c* riferimento e aggiorno la tabella con udate
009300050826     c                   if        §vpopfdr = 0
009400050826     c                   z-add     wdateu        o61drf
009500050826     c                   exsr      agg_tbe
009600050826     c                   else
009700050826     c* aggiungo alla data di rif. il numero dei giorni da trascorrere
009800050826     c* e confronto la data ottenuta con udate
009900050826     c                   move      §vpopfdr      dataiso
010000050829     c                   if        §vpopfgg = 0
010100050905     c                   z-add     5             §vpopfgg
010200050829     c                   endif
010300050826     c     dataiso       adddur    §vpopfgg:*d   dataiso
010400050826     c                   move      dataiso       §vpopfdr_x
010500050826     c* se la data del lavoro è >= alla ipotetica prossima data di riferimen
010600050826     c* to assumo come nuova data di riferimento la data del lavoro e
010700050826     c* aggiorno la tabella
010800050826     c                   if        wdateu >= §vpopfdr_x
010900050826     c                   z-add     wdateu        o61drf
011000050826     c                   exsr      agg_tbe
011100050826     c                   else
011200050826     c* se la data del lavoro è < alla ipotetica prossima data di riferiment
011300050826     c* mantengo come data di riferimento quella della tabella
011400050826     c                   z-add     §vpopfdr      o61drf
011500050826     c                   endif
011600050826     c                   endif
011700100618     c
011800100618     c                   if        %parms=3
011900100618     c                   exsr      contrGG
012000100618     c                   endif
012100050826     C*
012200090415     c     fine          tag
012300090415     c
012400970902     C                   SETON                                        LR
012500050826     C**********************************************************************
012600050826     c* Aggiornamento data riferimento su tabella "VPO" rek "PULIZIE_FIL"
012700050826     C**********************************************************************
012800050826     c     agg_tbe       begsr
012900050826     c     ktbe          chain     tntbe01l
013000050826     c                   if        %found(tntbe01l)
013100050826     c                   movel     tbeuni        dvpopulfil
013200050826     c                   z-add     o61drf        §vpopfdr
013300050829     c                   if        §vpopfgg = 0
013400050905     c                   z-add     5             §vpopfgg
013500050829     c                   endif
013600050826     c                   movel     dvpopulfil    tbeuni
013700050826     c                   update    tntbe000
013800050826     c                   endif
013900050826     c                   endsr
014000100618     C**********************************************************************
014100100618     c* Verifico se a udate devo aggiungere dei gg di pulizia
014200100618     C**********************************************************************
014300100618     c     ContrGG       begsr
014400100618     c                   clear                   dvpopuladd
014500100629     c                   movel     wdateu        wanno             4 0
014600100629     c     wanno         add       1             wannopiu          4 0
014700100618     c
014800100618     c                   eval      kkey='PULIZIE_ADD'
014900100629
015000100629     c     ktbe          setll     tntbe01l
015100100629     c     ktbe          reade     tntbe01l
015200100618     c                   dow       not %eof(tntbe01l)
015300100629     c                   if        tbeatb=' '
015400100629
015500100629     c                   movel     tbeuni        dvpopuladd
015600100629     c                   movel     tbeke2        §vpodsc           4 0
015700100629     c
015800100629     c* Decorrenza --> sempre anno in corso
015900100629     c                   z-add     §vpodec       wdec              8 0
016000100629     c                   movel     wanno         wdec
016100100629     c
016200100629     c* se Scadenza > Decorrenza --> imposto anno in corso nella scadenza
016300100629     c                   if        §vpodsc>=§vpodec
016400100629     c                   z-add     §vpodsc       wdsc              8 0
016500100629     c                   movel     wanno         wdsc
016600100629     c                   else
016700100629     c* Imposto  anno+1 nella scadenza
016800100629     c                   z-add     §vpodsc       wdsc              8 0
016900100629     c                   movel     wannopiu      wdsc
017000100629     c                   endif
017100100629     c
017200100629     c* Se la data scadenza < udate --> rileggo
017300100629     c* verifico la data di decorrenza <= oggi --> record trovato
017400100629     c                   if        wdsc >= wdateu  and wdec <=wdateu
017500100618     c                   leave
017600100618     c                   endif
017700100629     c
017800100629     c                   endif
017900100618     c*
018000100618     c                   clear                   dvpopuladd
018100100629     c     ktbe          reade     tntbe01l
018200100618     c                   enddo
018300100618
018400100618     c                   eval      parmpuladd=dvpopuladd
018500100618     c
018600100618     c                   endsr
