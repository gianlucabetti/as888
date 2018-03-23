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
004100100618     d kke2            s                   Like(tbeke2) inz
004200050826     d §vpopfdr_x      s                   Like(§vpopfdr)
004300050826     d dataiso         s               d   datfmt(*iso)
004400930322     C*---------------------------------------------------------------*
004500050826     C     KTbe          KLIST
004600050826     C                   KFLD                    kcod
004700050826     C                   KFLD                    kkey
004800100618     C     KTbe1         KLIST
004900100618     C                   KFLD                    kcod
005000100618     C                   KFLD                    kkey
005100100618     C                   KFLD                    kke2
005200050826     C
005300050826     C     *ENTRY        PLIST
005400050826     C                   PARM                    O61DRF            8 0
005500090415     C                   PARM                    i61rep            1
005600100618     C                   PARM                    parmpuladd
005700970902     C**
005800050826     C                   CLEAR                   O61DRF
005900090415     c
006000090415     c** Se passato 2° parametro vedo se è solo in reperimento
006100100618     c                   if        %parms>=2
006200090415     c                   movel     i61rep        solorep           1
006300090415     c                   else
006400090415     c                   clear                   soloREP
006500090415     c                   endif
006600050826     c*
006700050826     C                   TIME                    W0140            14 0
006800050826     C                   CLEAR                   WLBDAT
006900050826      * UDATE IN AAAAMMGG
007000050826     C                   MOVE      W0140         G02DAT
007100050826     C                   CALL      'XSRDA8'
007200050826     C                   PARM                    WLBDAT
007300050826     C                   Z-ADD     G02INV        WDateu            8 0
007400050826     c* chaino record di tabella contenente l'ultima data di riferimento
007500050826     c* utilizzata e il numero di giorni da trascorrere per il calcolo
007600050826     c* della nuova data di riferimento
007700050826     c                   clear                   dvpopulfil
007800050826     c     ktbe          chain(N)  tntbe01l
007900050826     c                   if        %found(tntbe01l)
008000050826     c                   movel     tbeuni        dvpopulfil
008100050826     c                   endif
008200090415     c
008300090415     c* se solo reperimento valorizzo il campo ed esco
008400090415     c                   if        soloREP='S'
008500090415     c
008600090415     c                   if        §vpopfdr = 0
008700090415     c                   z-add     wdateu        o61drf
008800090415     c                   else
008900090415     c                   z-add     §vpopfdr      o61drf
009000090415     c                   endif
009100090415     c                   goto      fine
009200090415     c
009300090415     c                   endif
009400090415     c
009500050826     c* se la data di riferimento in tabella è 0 assumo udate come data di
009600050826     c* riferimento e aggiorno la tabella con udate
009700050826     c                   if        §vpopfdr = 0
009800050826     c                   z-add     wdateu        o61drf
009900050826     c                   exsr      agg_tbe
010000050826     c                   else
010100050826     c* aggiungo alla data di rif. il numero dei giorni da trascorrere
010200050826     c* e confronto la data ottenuta con udate
010300050826     c                   move      §vpopfdr      dataiso
010400050829     c                   if        §vpopfgg = 0
010500050905     c                   z-add     5             §vpopfgg
010600050829     c                   endif
010700050826     c     dataiso       adddur    §vpopfgg:*d   dataiso
010800050826     c                   move      dataiso       §vpopfdr_x
010900050826     c* se la data del lavoro è >= alla ipotetica prossima data di riferimen
011000050826     c* to assumo come nuova data di riferimento la data del lavoro e
011100050826     c* aggiorno la tabella
011200050826     c                   if        wdateu >= §vpopfdr_x
011300050826     c                   z-add     wdateu        o61drf
011400050826     c                   exsr      agg_tbe
011500050826     c                   else
011600050826     c* se la data del lavoro è < alla ipotetica prossima data di riferiment
011700050826     c* mantengo come data di riferimento quella della tabella
011800050826     c                   z-add     §vpopfdr      o61drf
011900050826     c                   endif
012000050826     c                   endif
012100100618     c
012200100618     c                   if        %parms=3
012300100618     c                   exsr      contrGG
012400100618     c                   endif
012500050826     C*
012600090415     c     fine          tag
012700090415     c
012800970902     C                   SETON                                        LR
012900050826     C**********************************************************************
013000050826     c* Aggiornamento data riferimento su tabella "VPO" rek "PULIZIE_FIL"
013100050826     C**********************************************************************
013200050826     c     agg_tbe       begsr
013300050826     c     ktbe          chain     tntbe01l
013400050826     c                   if        %found(tntbe01l)
013500050826     c                   movel     tbeuni        dvpopulfil
013600050826     c                   z-add     o61drf        §vpopfdr
013700050829     c                   if        §vpopfgg = 0
013800050905     c                   z-add     5             §vpopfgg
013900050829     c                   endif
014000050826     c                   movel     dvpopulfil    tbeuni
014100050826     c                   update    tntbe000
014200050826     c                   endif
014300050826     c                   endsr
014400100618     C**********************************************************************
014500100618     c* Verifico se a udate devo aggiungere dei gg di pulizia
014600100618     C**********************************************************************
014700100618     c     ContrGG       begsr
014800100618     c                   clear                   dvpopuladd
014900100618     c
015000100618     c                   eval      kkey='PULIZIE_ADD'
015100100618     c                   eval      kke2=%editc(wdateu:'X')
015200100618     c     ktbe          setgt     tntbe01l
015300100618     c     ktbe          readpe    tntbe01l
015400100618     c                   dow       not %eof(tntbe01l)
015500100618     c* Se la data scadenza < udate --> fine lettura non c'e' aggiunta
015600100618     c                   if        tbeke2<kke2
015700100618     c                   leave
015800100618     c                   else
015900100618     c* verifico la data di decorrenza <= oggo --> record trovato
016000100618     c                   movel     tbeuni        dvpopuladd
016100100618     c                   if        §vpodec<=wdateu
016200100618     c                   leave
016300100618     c                   endif
016400100618     c                   endif
016500100618     c*
016600100618     c                   clear                   dvpopuladd
016700100618     c     ktbe          readpe    tntbe01l
016800100618     c                   enddo
016900100618
017000100618     c                   eval      parmpuladd=dvpopuladd
017100100618     c
017200100618     c                   endsr
