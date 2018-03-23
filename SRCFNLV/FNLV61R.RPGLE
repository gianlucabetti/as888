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
003600110104     d
003700110104     D                 DS                  INZ
003800110104     D Wmmgg                   1      4  0
003900110104     D  wmm                    1      2  0
004000110104     D  wgg                    3      4  0
004100050712     d
004200110104     D                 DS
004300110104     D  wdateu                 1      8  0
004400110104     D  wuaa                   1      4  0
004500110104     D  wumm                   5      6  0
004600110104     D  wugg                   7      8  0
004700110104     d
004800100618     d dvpopuladd    e ds
004900100618     d parmpuladd    e ds                  extname(dvpopuladd) prefix(P)
005000050826     d kcod            s                   Like(tbecod) inz('VPO')
005100050826     d kkey            s                   Like(tbeke1) inz('PULIZIE_FIL    ')
005200050826     d §vpopfdr_x      s                   Like(§vpopfdr)
005300050826     d dataiso         s               d   datfmt(*iso)
005400930322     C*---------------------------------------------------------------*
005500050826     C     KTbe          KLIST
005600050826     C                   KFLD                    kcod
005700050826     C                   KFLD                    kkey
005800100629     C*---------------------------------------------------------------*
005900100629
006000050826     C     *ENTRY        PLIST
006100050826     C                   PARM                    O61DRF            8 0
006200090415     C                   PARM                    i61rep            1
006300100618     C                   PARM                    parmpuladd
006400970902     C**
006500050826     C                   CLEAR                   O61DRF
006600090415     c
006700090415     c** Se passato 2° parametro vedo se è solo in reperimento
006800100618     c                   if        %parms>=2
006900090415     c                   movel     i61rep        solorep           1
007000090415     c                   else
007100090415     c                   clear                   soloREP
007200090415     c                   endif
007300050826     c*
007400050826     C                   TIME                    W0140            14 0
007500050826     C                   CLEAR                   WLBDAT
007600050826      * UDATE IN AAAAMMGG
007700050826     C                   MOVE      W0140         G02DAT
007800050826     C                   CALL      'XSRDA8'
007900050826     C                   PARM                    WLBDAT
008000050826     C                   Z-ADD     G02INV        WDateu            8 0
008100050826     c* chaino record di tabella contenente l'ultima data di riferimento
008200050826     c* utilizzata e il numero di giorni da trascorrere per il calcolo
008300050826     c* della nuova data di riferimento
008400050826     c                   clear                   dvpopulfil
008500050826     c     ktbe          chain(N)  tntbe01l
008600050826     c                   if        %found(tntbe01l)
008700050826     c                   movel     tbeuni        dvpopulfil
008800050826     c                   endif
008900090415     c
009000090415     c* se solo reperimento valorizzo il campo ed esco
009100090415     c                   if        soloREP='S'
009200090415     c
009300090415     c                   if        §vpopfdr = 0
009400090415     c                   z-add     wdateu        o61drf
009500090415     c                   else
009600090415     c                   z-add     §vpopfdr      o61drf
009700090415     c                   endif
009800090415     c                   goto      fine
009900090415     c
010000090415     c                   endif
010100090415     c
010200050826     c* se la data di riferimento in tabella è 0 assumo udate come data di
010300050826     c* riferimento e aggiorno la tabella con udate
010400050826     c                   if        §vpopfdr = 0
010500050826     c                   z-add     wdateu        o61drf
010600050826     c                   exsr      agg_tbe
010700050826     c                   else
010800050826     c* aggiungo alla data di rif. il numero dei giorni da trascorrere
010900050826     c* e confronto la data ottenuta con udate
011000050826     c                   move      §vpopfdr      dataiso
011100050829     c                   if        §vpopfgg = 0
011200050905     c                   z-add     5             §vpopfgg
011300050829     c                   endif
011400050826     c     dataiso       adddur    §vpopfgg:*d   dataiso
011500050826     c                   move      dataiso       §vpopfdr_x
011600050826     c* se la data del lavoro è >= alla ipotetica prossima data di riferimen
011700050826     c* to assumo come nuova data di riferimento la data del lavoro e
011800050826     c* aggiorno la tabella
011900050826     c                   if        wdateu >= §vpopfdr_x
012000050826     c                   z-add     wdateu        o61drf
012100050826     c                   exsr      agg_tbe
012200050826     c                   else
012300110104     c* se la data del
012400050826     c* mantengo come data di riferimento quella della tabella
012500050826     c                   z-add     §vpopfdr      o61drf
012600050826     c                   endif
012700050826     c                   endif
012800100618     c
012900100618     c                   if        %parms=3
013000100618     c                   exsr      contrGG
013100100618     c                   endif
013200050826     C*
013300090415     c     fine          tag
013400090415     c
013500970902     C                   SETON                                        LR
013600050826     C**********************************************************************
013700050826     c* Aggiornamento data riferimento su tabella "VPO" rek "PULIZIE_FIL"
013800050826     C**********************************************************************
013900050826     c     agg_tbe       begsr
014000050826     c     ktbe          chain     tntbe01l
014100050826     c                   if        %found(tntbe01l)
014200050826     c                   movel     tbeuni        dvpopulfil
014300050826     c                   z-add     o61drf        §vpopfdr
014400050829     c                   if        §vpopfgg = 0
014500050905     c                   z-add     5             §vpopfgg
014600050829     c                   endif
014700050826     c                   movel     dvpopulfil    tbeuni
014800050826     c                   update    tntbe000
014900050826     c                   endif
015000050826     c                   endsr
015100100618     C**********************************************************************
015200100618     c* Verifico se a udate devo aggiungere dei gg di pulizia
015300100618     C**********************************************************************
015400100618     c     ContrGG       begsr
015500100618     c                   clear                   dvpopuladd
015600100629     c                   movel     wdateu        wanno             4 0
015700110104     c     wanno         sub       1             wannomeno         4 0
015800100618     c
015900100618     c                   eval      kkey='PULIZIE_ADD'
016000100629
016100100629     c     ktbe          setll     tntbe01l
016200100629     c     ktbe          reade     tntbe01l
016300100618     c                   dow       not %eof(tntbe01l)
016400100629     c                   if        tbeatb=' '
016500100629
016600100629     c                   movel     tbeuni        dvpopuladd
016700100629     c                   movel     tbeke2        §vpodsc           4 0
016800100629     c
016900110104     c* Decorrenza --> se il mese in corso è più piccolo di qeullo in tabella
017000110104     c*    imposto anno - 1 , altrimenti anno in corso
017100110104     c                   z-add     §vpodec       wmmgg
017200110104     c                   if        wumm >= wmm
017300110104     c                   z-add     §vpodec       wdec              8 0
017400100629     c                   movel     wanno         wdec
017500110104     c                   else
017600110104     c                   z-add     §vpodec       wdec              8 0
017700110104     c                   movel     wannomeno     wdec
017800110104     c                   endif
017900100629     c
018000110104     c                   movel     wdec          wannodec          4 0
018100100629     c* se Scadenza > Decorrenza --> imposto anno in corso nella scadenza
018200100629     c                   if        §vpodsc>=§vpodec
018300100629     c                   z-add     §vpodsc       wdsc              8 0
018400110104     c                   movel     wannodec      wdsc
018500100629     c                   else
018600100629     c* Imposto  anno+1 nella scadenza
018700100629     c                   z-add     §vpodsc       wdsc              8 0
018800110104     c                   add       1             wannodec
018900110104     c                   movel     wannodec      wdsc
019000100629     c                   endif
019100100629     c
019200100629     c* Se la data scadenza < udate --> rileggo
019300100629     c* verifico la data di decorrenza <= oggi --> record trovato
019400100629     c                   if        wdsc >= wdateu  and wdec <=wdateu
019500100618     c                   leave
019600100618     c                   endif
019700100629     c
019800100629     c                   endif
019900100618     c*
020000100618     c                   clear                   dvpopuladd
020100100629     c     ktbe          reade     tntbe01l
020200100618     c                   enddo
020300100618
020400100618     c                   eval      parmpuladd=dvpopuladd
020500100618     c
020600100618     c                   endsr
