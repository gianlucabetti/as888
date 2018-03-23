000100970526     H*------------------------------------------------------------------------*
000200040628     H* STATISTICA VAS - ESITI CONSEGNE T&T (WURTH)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800040628     Ftis7w4td  cf   e             WORKSTN INFDS(DEVDS1)
000900040628     F                                     SFILE(S7W4DMS:MSGULT)
001000020611     Fcnaco00f  IF   E           K DISK    RENAME(cnaco000:cnacoXXX)
001100050519     Ftabel00f  IF   E           K DISK
001200050617     Ftntbe01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D* MESSAGGI
001700970526     D*------------
001800000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001900050519     D*------------
002000050519     D tute            S                   LIKE(tblkut) inz(1)
002100050519     D tcod            S                   LIKE(tblcod)
002200050519     D tkey            S                   LIKE(tblkey)
002300050519     D datmin          S                   LIKE(g08dat)
002400050519     d dataiso         s               d   datfmt(*iso)
002500970526     D*------------
002600970526     D* DESCRIZIONI TASTI FUNZIONALI
002700970526     D*------------
002800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* FUNZIONI DA CARICARE A VIDEO
003100970526     D*------------
003200970526     D FUV             S             19    DIM(24)
003300970526     D*------------------------------------------------------------------------*
003400970526     D* INPUT
003500970526     D*------------------------------------------------------------------------*
003600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003700970526     D*------------
003800970526     D FUDDS           DS
003900970526     D  FUDDE1                 1      1
004000970526     D  FUDFUN                 2      3
004100970526     D  FUDDE2                 4     19
004200970526     D  FUDFI0                20     34
004300970526     D  FUDATT                35     35
004400970526     D  FUDFI1                36     80
004500970526     D  FUDDES                 1     19
004600970526     D  FUDRIG                 1     80
004700970526     D*------------
004800970526     D* DS GESTIONE TASTI FUNZIONALI
004900970526     D*------------
005000970526     D DSKEY         E DS
005100970526     D DEVDS1          DS
005200970526     D  KEY                  369    369
005300970526     D*------------
005400970526     D* DS "XMSG" - GESTIONE MESSAGGI
005500970526     D*------------
005600970526     D DSMSG         E DS
005700970528     D*------------------
005800970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900970528     D*------------------
006000970528     D WLBDA8          DS                  INZ
006100970528     D  G08DAT                 1      8  0
006200970528     D  G08INV                 9     16  0
006300970528     D  G08ERR                17     17
006400970528     D  G08TGI                18     22  0
006500991216     D*------------------
006600991216     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
006700991216     D*------------------
006800991216     D DSXC          e DS                  extname(XCLIRDS)
006900050518     D*------------------
007000050518     D DTLE          e DS
007100050519     D*------------------
007200050519     D DS$3          e DS
007300050617     D*------------------
007400050617     D DAVC          e DS
007500050518     D*------------------
007600050518     D* DS PER TIBS02R - GESTIONE TNTBE00F
007700050518     D*------------------
007800050518     D tibs02ds      E DS
007900020611     D*------------------
008000050518     D* DS PER TIBS10R - clienti unificati - ricerca
008100050518     D*------------------
008200050518     D tibs10ds      E DS
008300050518     D*------------------
008400991216     D                 DS
008500991216     D dsksn                   1      4p 0
008600991216     D dsksa                   1      4
008700970526     D*------------
008800970526     D* ARCHITETTURA
008900970526     D*------------
009000970526     D KPJBA         E DS
009100040628     D*------------
009200040628     D* DS X ESECUZIONE STATISTICA
009300040628     D*------------
009400040628     D TIS1W2IDS     E DS                  INZ
009500991215     D*------------
009600991215     D* TABELLE "XCLIR"- RICERCA CLIENTI
009700991215     D*------------
009800040628     D SKKSC           s              7  0 dim(30)
009900040628     D SKKSA           s              4    dim(30)
010000991216     D*------------------
010100991216     D BS69DS        e ds                  extname(TIBS69DS)
010200991216     D ACODS         e ds                  extname(cnaco00f)
010300991216     D INDDS         e ds                  extname(cnind00f)
010400991216     D CLPDS         e ds                  extname(cnclp00f)
010500991216     D CLSDS         e ds                  extname(fncls00f)
010600991214     D*------------------
010700991214     D* DS X PASSAGGIO PARAMETRI
010800991214     D*------------------
010900050210     D PARAM           DS                  INZ
011000040628     D  PRMTIS1W2IDS                       LIKE(TIS1W2IDS)
011100040628     D  PRMDAT                       10
011200040628     D  PRMORA                        8
011300050518     D  PRMSUF                        1
011400050518     D  PRMTLG                        2
011500050614     D  PRMFILE                       1
011600020923     D*-------------------
011700020923     D* DS REPERIMENTO DATI UTENTE
011800020923     D*-------------------
011900020923     D TIBS34DS      E DS                                                       *Profili utente
012000020923     D DDATIUTE      E DS                                                       *Dati utente
012100020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012200970526     C*------------------------------------------------------------------------*
012300000303     C* MAIN LINES
012400970526     C*------------------------------------------------------------------------*
012500970526     C*
012600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013000970526     C*
013100970526     C* EMETTE FORMATO VIDEO
013200970526     C                   EXSR      BUTFMR
013300970526     C*
013400970526     C* GESTIONE TASTI FUNZIONALI
013500970526     C                   EXSR      FUNCON
013600970526E   2C                   ENDDO
013700970526     C*
013800970526     C* FINE PGM
013900970526     C                   SETON                                        LR
014000050518     c*
014100050518     C                   CLEAR                   tibs02ds
014200050518     C                   MOVEL     'C'           T02TLA
014300050518     C                   CALL      'TIBS02R'
014400050518     C                   PARM                    KPJBA
014500050518     C                   PARM                    tibs02ds
014600970526     C*------------------------------------------------------------------------*
014700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     BUTFMR        BEGSR
015000970526     C*
015100970526     C* EMETTE LA TESTATA
015200040628     C                   WRITE     S7W4D01
015300970526     C*
015400970526     C* SPEDISCE EVENTUALE MESSAGGIO
015500970526     C                   EXSR      SNDMSG
015600970526     C*
015700970526     C* EMETTE LE FUNZIONI
015800040628     C                   WRITE     S7W4D02
015900970526     C*
016000970526     C* EMETTE IL VIDEO
016100040628     C                   EXFMT     S7W4D03
016200970526     C*
016300991214     C* REIMPOSTA FLAG DI ERRORE
016400991214     C                   MOVEL     '0'           ERRL00
016500970526     C*
016600970526     C* REIMPOSTA INDICATORI DI ERRORE
016700050518     C                   SETOFF                                       545556
016800050518     C                   SETOFF                                       45
016900970526     C*
017000970526     C                   ENDSR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017300970526     C*------------------------------------------------------------------------*
017400970526     C     SNDMSG        BEGSR
017500970526     C*
017600970526     C* PULISCE IL SFILE MESSAGGI
017700970526     C                   MOVEL     '1'           *IN40
017800040628     C                   WRITE     S7W4DMC
017900970526     C                   MOVEL     '0'           *IN40
018000970526     C*
018100970526     C* VISUALIZZA IL MESSAGGIO
018200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018300970526     C                   CALL      'XMSG'        XMSGPA
018400020611     C     DSMSER        IFEQ      *ZEROS
018500970526     C                   MOVEL     DSMSKE        MSGKEY
018600020611     C                   ENDIF
018700970526     C                   Z-ADD     1             MSGULT
018800040628     C                   WRITE     S7W4DMS
018900040628     C                   WRITE     S7W4DMC
019000970526     C*
019100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019200970526     C                   MOVEL     *BLANKS       DSMSMS
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* ENTGES - GESTIONE ENTER -> INVIO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     ENTGES        BEGSR
019900970526     C*
020000970526     C* CONTROLLA I CAMPI IMMESSI
020100970526     C                   EXSR      DECODI
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F03GES - GESTIONE F03 -> FINE
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F03GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F03"
021000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400991214     C* F06GES - GESTIONE F06 -> CONFERMA
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F06GES        BEGSR
021700970526     C*
021800991214     C* CONTROLLA I CAMPI IMMESSI
021900991214     C                   EXSR      DECODI
022000020611     C*
022100991214     C* ESEGUE ROUTINE DI CONFERMA
022200020611     C     ERRL00        IFEQ      '0'
022300991214     C                   EXSR      ESEGUI
022400030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
022500991214     C                   ENDIF
022600970526     C*
022700991214     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900970526     C* F12GES - GESTIONE F12 -> RITORNO
023000970526     C*------------------------------------------------------------------------*
023100970526     C     F12GES        BEGSR
023200970526     C*
023300970526     C* IMPOSTA "PREMUTO F12"
023400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023900970526     C*------------------------------------------------------------------------*
024000970526     C     F24GES        BEGSR
024100970526     C*
024200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024300970526     C*
024400970526     C                   ENDSR
024500991214     C*------------------------------------------------------------------------*
024600991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
024700991214     C*------------------------------------------------------------------------*
024800040628     C     PASPAR        BEGSR
024900991214     C*
025000040628     C                   CLEAR                   TIS1W2IDS
025100040628     C*
025200040628     C                   MOVE(P)   VIDKSC        wVIDKSCnum        8 0
025300040628     C                   EVAL      RQSCID = VIDSGI
025400040628     C                   MOVE      wVIDKSCnum    KSC
025500040628     C                   MOVE      VIDDAI        wVIDDAIAlfa       8
025600040628     C                   EVAL      DIN = %subst(wVIDDAIAlfa:1:2)+'/'+
025700040628     C                                   %subst(wVIDDAIAlfa:3:2)+'/'+
025800040628     C                                   %subst(wVIDDAIAlfa:5:4)
025900040628     C                   MOVE      VIDDAF        wVIDDAFAlfa       8
026000040628     C                   EVAL      DFI = %subst(wVIDDAFAlfa:1:2)+'/'+
026100040628     C                                   %subst(wVIDDAFAlfa:3:2)+'/'+
026200040628     C                                   %subst(wVIDDAFAlfa:5:4)
026300040628     C                   EVAL      LNA = VIDLNA
026400040628     C                   EVAL      CKC = VIDCKC
026500040628     C                   EVAL      CKG = VIDCKG
026600040628     C                   EVAL      CKA = VIDCKA
026700040628     C                   EVAL      CKM = VIDCKM
026800050210     C                   EVAL      CKP = VIDCKP
026900050210     C                   EVAL      CKT = VIDCKT
027000050210     C                   EVAL      CKL = VIDCKL
027100991214     C*
027200040628     C* Edito la data e ora corrente
027300040628     C                   MOVEL     WN8           wDATCORAlfa       8
027400040628     C                   MOVEL     ORACOR        wORACORAlfa       6
027500040628     C                   EVAL      PRMDAT = %subst(wDATCORAlfa:1:2)+'/'+
027600040628     C                                      %subst(wDATCORAlfa:3:2)+'/'+
027700040628     C                                      %subst(wDATCORAlfa:5:4)
027800040628     C                   EVAL      PRMORA = %subst(wORACORAlfa:1:2)+':'+
027900040628     C                                      %subst(wORACORAlfa:3:2)+':'+
028000040628     C                                      %subst(wORACORAlfa:5:2)
028100040628     C*
028200040628     C                   EVAL      PRMTIS1W2IDS = TIS1W2IDS
028300050518     c*
028400050614     c                   eval      prmsuf  = vidsuf
028500050614     c                   eval      prmtlg  = vidtlg
028600050614     c                   eval      prmfile = vidpf
028700040628     C*
028800991214     C                   ENDSR
028900970526     C*------------------------------------------------------------------------*
029000991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
029100970526     C*------------------------------------------------------------------------*
029200991214     C     ESEGUI        BEGSR
029300991214     C*
029400991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
029500040628     C                   MOVEL     'S7W5'        KCOAZ
029600991214     C                   EXSR      PASPAR
029700040628     C                   MOVEL     PARAM         KPJBU
029800060217     C                   CALL      'BCH10'
029900060217 MOM C***                CALL      'TIS7W5C'
030000991214     C                   PARM                    KPJBA
030100991214     C*
030200970526     C                   ENDSR
030300970526     C*------------------------------------------------------------------------*
030400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030500970526     C*------------------------------------------------------------------------*
030600970526     C     DECODI        BEGSR
030700000306     C*
030800050518     C                   SETOFF                                       545545
030900050617     C                   SETOFF                                       56
031000050617     C                   SETON                                        46
031100000306     C*
031200050518     C*--------------------
031300050518     C* CLIENTE SINGOLO/UNIFICANTE
031400050518     c* Se scelto U=Unificante attivo ed imposto campo tipo legame
031500050518     C*--------------------
031600050518    1c     errl00        ifeq      '0'
031700050518    2c                   if        vidsuf = 'U' and *in44 = *on
031800050518     c                   eval      vidtlg = 'ST'
031900050518     c                   setoff                                       44
032000050518     c                   eval      errl00 = '1'
032100050518   x2c                   else
032200050518    3c                   if        vidsuf = 'S' and *in44 = *off
032300050518     c                   seton                                        44
032400050518     c                   eval      vidtlg = *blanks
032500050518     c                   eval      errl00 = '1'
032600050518    3c                   endif
032700050518    2c                   endif
032800050518    1c                   endif
032900050518     C*--------------------
033000050518     C* TIPO LEGAME
033100050518     C*--------------------
033200050518    1c                   if        *in44 = *off
033300050518    2c     errl00        ifeq      '0'
033400050518     c* ricerca
033500050518     C     '?'           SCAN      VIDtlg                                 30
033600050518    3C     *IN30         IFEQ      *ON
033700050518     c                   Clear                   tibs02ds
033800050518     c                   Eval      T02Mod = 'R'
033900050518     c                   Eval      T02Sif = Knsif
034000050518     c                   Eval      T02Cod = 'TLE'
034100050518     c                   Call      'TIBS02R'
034200050518     c                   Parm                    Kpjba
034300050518     c                   Parm                    tibs02ds
034400050518     c                   Movel     T02Ke1        VidTlg
034500050518     c                   Eval      errl00 = '1'
034600050518    3c                   endif
034700050518     c* errore se tipo legame non immesso
034800050518    3c     errl00        ifeq      '0'
034900050518     c     vidtlg        andeq     *blanks
035000050518     c                   seton                                        45
035100050518     C                   MOVEL     MSG(18)       DSMSMS
035200050518     c                   movel     '1'           errl00
035300050518    3c                   endif
035400050518     c* controllo esistenza in tabella
035500050518    3c     errl00        ifeq      '0'
035600050518     C                   CLEAR                   tibs02ds
035700050518     c                   clear                   dtle
035800050518     c                   clear                   viddtl
035900050518     C                   MOVEL     'C'           T02MOD
036000050518     C                   MOVEL     KNSIF         T02SIF
036100050518     C                   MOVEL     'TLE'         T02COD
036200050518     c                   movel(P)  vidtlg        t02ke1
036300050518     C                   CALL      'TIBS02R'
036400050518     C                   PARM                    KPJBA
036500050518     C                   PARM                    tibs02ds
036600050518    4C                   IF        T02ERR = *BLANKS
036700050518     C                   MOVEL     T02uni        dtle
036800050518     c                   else
036900050518     C                   SETON                                        45
037000050518     C                   MOVEL     MSG(18)       DSMSMS
037100050518     c                   movel     '1'           errl00
037200050518    4C                   ENDIF
037300050518     c                   movel     §tledesc      viddtl
037400050518    3c                   endif
037500050518    2c                   endif
037600050518    1c                   endif
037700050518     c
037800991215     C*--------------------
037900000306     C* CLIENTI
038000991215     C*--------------------
038100991215     C     ERRL00        IFEQ      '0'
038200991216     C     '?'           SCAN      VIDKSC                                 30
038300991215     C     *IN30         IFEQ      *ON
038400991215     C                   CLEAR                   DSXC
038500020923     C                   Z-ADD     DUTKCI        DXCCAP
038600991215     C                   CALL      'XCLIR'
038700991215     C                   PARM                    DSXC
038800050519     C                   PARM                    VIDTLG
038900991215     C     DXCERR        ifne      '1'
039000991216     C*-------------
039100040628     C                   MOVEA     DXCKSC        SKKSA                      ALF
039200991216DO  2C     1             DO        30            J                 3 0
039300040628     C                   MOVE      SKKSA(J)      DSKSA                      F
039400040628     C                   Z-ADD     DSKSN         SKKSC(J)                   M
039500991216E   2C                   ENDDO
039600991216     C*-------------
039700040628     C                   MOVE      SKKSC(1)      VIDKSC
039800991215     C                   ENDIF
039900991215     C                   ENDIF
040000991216     C     VIDKSC        IFNE      *zeros
040100991216     C     VIDKSC        ANDNE     *blanks
040200020923     C                   Z-ADD     DUTKCI        kclkcc
040300020611     C                   MOVEL     VIDKSC        kclksc
040400020611     C     keycli        chain     cnaco00f                           99
040500991215     C     *IN99         ifeq      *off
040600991216     C                   MOVEL     acorag        VIDRAG
040700020611     C                   endif
040800050518     c                   else
040900050518     c* errore se non immesso
041000050518     c                   clear                   vidrag
041100050518     c                   seton                                        56
041200050518     C                   MOVEL     MSG(19)       DSMSMS
041300050518     c                   Eval      errl00 = '1'
041400000306     C                   ENDIF
041500050518     c* se richiesto cliente unificante e il cliente immesso è figlio
041600050518     c* propongo in automatico il relativo codice padre
041700050518     C     ERRL00        IFEQ      '0'
041800050518     c     vidsuf        andeq     'U'
041900050518     c                   clear                   tibs10ds
042000050518     c                   eval      d10tle = vidtlg
042100050518     c                   eval      d10paf = 'P'
042200050518     c                   move      vidksc        d10cod
042300050518     c                   z-add     datcor        d10drf
042400050518     c                   call      'TIBS10R'
042500050518     c                   Parm                    Tibs10ds
042600050518     c                   If        d10err =  *Blanks
042700050518     c                   move      d10cop        vidksc
042800050518     c                   endif
042900050518     c                   endif
043000050518     c
043100000306     C                   ENDIF
043200991015     C*--------------------
043300020611     C* DATA INIZIO
043400991015     C*--------------------
043500020611     C     *LIKE         DEFINE    VIDDAI        DAIVID
043600020611     C     ERRL00        IFEQ      '0'
043700020611     C                   Z-ADD     VIDDAI        WDA8
043800020611     C                   Z-ADD     *ZEROS        DA8W
043900991015     C                   EXSR      CHKDA8
044000020611     C                   Z-ADD     WDA8          VIDDAI
044100020611     C                   Z-ADD     DA8W          DAIVID
044200020611     C     ERRL00        IFEQ      '1'
044300991214     C                   SETON                                        54
044400020611     C                   MOVEL     MSG(14)       DSMSMS
044500050519     c                   else
044600050519     c* la data inizio deve essere > della data spedizione più alta presente
044700050519     c* nel titasp (tale data è memorizzata in tabella $3 campo §$3D10)
044800050519     c* In sostanza vanno bene solo le date spedizioni presenti nel titas10
044900050519     c* o nel titas0. Non si accettano date spedizioni più vecchie in
045000050519     c* quanto il batch estrae i dati solo dal titas10 e dal titas0
045100070222     c***                if        daivid <= §$3d10
045200070222     C***                SETON                                        54
045300050519     c* edito la data da visualizzare nel messaggio
045400070222     c***                eval      %subst(msg(20):62:10) =
045500070222     c***                          %editw(datmin:'  /  /    ')
045600070222     C***                MOVEL     MSG(20)       DSMSMS
045700070222     c***                eval      errl00 = '1'
045800070222     c***                endif
045900020611     C                   ENDIF
046000020611     C                   ENDIF
046100970528     C*--------------------
046200020611     C* DATA FINE
046300970528     C*--------------------
046400020611     C     *LIKE         DEFINE    VIDDAF        DAFVID
046500020611     C     ERRL00        IFEQ      '0'
046600020611     C                   Z-ADD     VIDDAF        WDA8
046700020611     C                   Z-ADD     *ZEROS        DA8W
046800970528     C                   EXSR      CHKDA8
046900020611     C                   Z-ADD     WDA8          VIDDAF
047000020611     C                   Z-ADD     DA8W          DAFVID
047100020611     C     ERRL00        IFEQ      '1'
047200991214     C                   SETON                                        55
047300970528     C                   MOVEL     MSG(14)       DSMSMS
047400020611     C                   ENDIF
047500020611     C                   ENDIF
047600020611     C*
047700970528     C* SCADENZA > DECORRENZA
047800020611     C     ERRL00        IFEQ      '0'
047900020611     C     DAFVID        IFLT      DAIVID
048000020611     C                   SETON                                        5455
048100970528     C                   MOVEL     MSG(15)       DSMSMS
048200970528     C                   MOVEL     '1'           ERRL00
048300020611     C                   ENDIF
048400020611     C                   ENDIF
048500050617     C*
048600050617     C* VERIFICA ABILITAZIONE CLIENTE X OUTPUT STATISTICA SU FILE
048700050617     C     ERRL00        IFEQ      '0'
048800050617     C     VIDKSC        IFNE      *ZEROS
048900050617     C                   EVAL      tbeCOD = 'AVC'
049000050617     C                   EVAL      tbeKE1 = VIDKSC
049100050617     C     keytbe1_P     CHAIN     TNTBE01L
049200050617     C                   IF        %found(TNTBE01L)
049300050617     C                   EVAL      DAVC = tbeUNI
049400050617     C                   IF        §AVCS7W5 = 'S'
049500050617     C                   SETOFF                                       46
049600050617     C                   ENDIF
049700050617     C                   ENDIF
049800050617     C                   ENDIF
049900050617     C                   ENDIF
050000050617     C                   IF        *IN46 = *ON
050100050617     C                   EVAL      VIDPF = *BLANKS
050200050617     C                   ENDIF
050300000306     C*
050400970528     C                   ENDSR
050500970528     C*------------------------------------------------------------------------*
050600970528     C* CHKDA8 - CONTROLLA LA DATA (8)
050700970528     C*------------------------------------------------------------------------*
050800970528     C     CHKDA8        BEGSR
050900970528     C*
051000020611     C     *LIKE         DEFINE    VIDDAI        WDA8
051100020611     C     *LIKE         DEFINE    VIDDAF        DA8W
051200970528     C*
051300970528     C                   Z-ADD     WDA8          G08DAT
051400970528     C                   Z-ADD     *ZEROS        G08INV
051500970528     C                   MOVEL     '0'           G08ERR
051600970528     C                   CALL      'XSRDA8'
051700970528     C                   PARM                    WLBDA8
051800970528     C                   Z-ADD     G08DAT        WDA8
051900970528     C                   Z-ADD     G08INV        DA8W
052000020611     C     G08ERR        IFEQ      '1'
052100970528     C                   MOVEL     '1'           ERRL00
052200020611     C                   ENDIF
052300970528     C*
052400970528     C                   ENDSR
052500970526     C*------------------------------------------------------------------------*
052600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
052700970526     C*------------------------------------------------------------------------*
052800970526     C     AZZVID        BEGSR
052900970526     C* CAMPI
053000050518     c                   movel     'S'           vidsuf
053100050518     c                   clear                   vidtlg
053200050518     c                   seton                                        44
053300020611     C                   Z-ADD     *ZEROS        VIDDAI
053400020611     C                   Z-ADD     *ZEROS        VIDDAF
053500040628     C                   Z-ADD     999           VIDLNA
053600040628     C                   MOVEL     'S'           VIDCKC
053700040628     C                   MOVEL     'S'           VIDCKM
053800040628     C                   MOVEL     'S'           VIDCKG
053900040628     C                   MOVEL     'S'           VIDCKA
054000050210     C                   MOVEL     'S'           VIDCKP
054100050210     C                   MOVEL     'S'           VIDCKT
054200050210     C                   MOVEL     'S'           VIDCKL
054300040628     C
054400970526     C* DECODIFICHE
054500991216     C                   MOVEL     *BLANKS       VIDRAG
054600050518     C                   MOVEL     *BLANKS       VIDDTL
054700970526     C*
054800970526     C                   ENDSR
054900970526     C*------------------------------------------------------------------------*
055000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
055100970526     C*------------------------------------------------------------------------*
055200970526     C     IMPFUN        BEGSR
055300970526     C*
055400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
055500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
055600970526     C                   EXSR      FUNGES
055700970526     C*
055800970526     C                   ENDSR
055900020923     C*--------------------------------------------------------------------------------------------*
056000020923     C* REPERISCE I DATI UTENTE
056100020923     C*--------------------------------------------------------------------------------------------*
056200020923     C     REPDATIUTE    BEGSR
056300020923     C*
056400020923     C* INIZIALIZZA VARIABILI DI WRK
056500020923     C                   CLEAR                   TIBS34DS
056600020923     C                   CLEAR                   AZUTEDS
056700020923     C                   CLEAR                   DDATIUTE
056800020923     C*
056900020923     C     *DTAARA       DEFINE    §azute        azuteds
057000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
057100020923     C                   IN(E)     *DTAARA
057200020923if  1C                   IF        %Error
057300020923     c                   EVAL      I34Tla = 'L'
057400020923     C                   CALL      'TIBS34R'
057500020923     C                   PARM                    Tibs34Ds
057600020923     C                   IN        *DTAARA
057700020923e   1C                   ENDIF
057800020923     C*
057900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
058000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
058100020923     C*
058200020923     C                   ENDSR
058300970526     C*------------------------------------------------------------------------*
058400970526     C* *INZSR - OPERAZIONI INIZIALI
058500970526     C*------------------------------------------------------------------------*
058600970526     C     *INZSR        BEGSR
058700991217     C*
058800991217     C     *ENTRY        PLIST
058900991217     C                   PARM                    KPJBA
059000020923     C*
059100020923     C* REPERISCE I DATI DELL'UTENTE
059200020923     C                   EXSR      REPDATIUTE
059300050617     C*
059400050617     C* IMPOSTO SUBITO ALCUNI INDICATORI D WRK
059500050617     C                   SETON                                        46
059600050503     C*
059700050503     C* VERIFICO L'UTENTE CHE EFFETTUA LA RICHIESTA, SE NN 'EDP*' => INIBISCO ALCUNI CAMPI A VIDEO
059800050503     C                   IF        %subst(%trim(KNMUS):1:3) <> 'EDP'
059900050503     C                   SETON                                        50
060000050503     C                   ELSE
060100050503     C                   SETOFF                                       50
060200050503     C                   ENDIF
060300991215     C*
060400000306     C* LETTURA cnaco31c/cnaco00f
060500991215     C     keycli        KLIST                                                  *di cnaco/ind/clp
060600991215     C                   KFLD                    kclkut            1 0          -utente
060700991215     C                   KFLD                    kclkcc            4 0          -capoconto
060800991215     C                   KFLD                    kclksc            7 0          -codice
060900991216     C                   z-add     1             kclkut
061000050519     C* Definizione chiavi - TABEL00F
061100050519     C     keytab        KLIST
061200050519     C                   KFLD                    tute
061300050519     C                   KFLD                    tcod
061400050519     C                   KFLD                    tkey
061500050617     C* Definizione chiavi - TNTBE01L
061600050617     C     keytbe1_P     KLIST
061700050617     C                   KFLD                    tbeCOD
061800050617     C                   KFLD                    tbeKE1
061900970526     C*
062000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
062100970526     C     XMSGPA        PLIST
062200970526     C                   PARM                    DSMSMS
062300970526     C                   PARM                    DSMSPG
062400970526     C                   PARM                    DSMSKE
062500970526     C                   PARM                    DSMSER
062600970526     C*
062700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
062800970526     C                   CALL      'XKEY'
062900970526     C                   PARM                    DSKEY
063000970526     C*---
063100970526     C* VARIABILI NON RIFERITE AL DATA BASE
063200970526     C*---
063300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
063400991214     C                   MOVEL     *BLANKS       DEPOP1            3
063500970526     C*---
063600970526     C* VARIABILI DI CONTROLLO
063700970526     C*---
063800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
063900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
064000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
064100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
064200970526     C*
064300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
064400040628     C                   MOVEL     'TIS7W4R'     DSMSPG
064500040628     C                   MOVEL     'TIS7W4R'     MSGQUE
064600970526     C                   MOVEL     *BLANKS       MSGKEY
064700970526     C                   MOVEL     *BLANKS       DSMSMS
064800970528     C*
064900970528     C* CALCOLA LA DATA CORRENTE
065000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
065100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
065200040628     C                   MOVEL     WN14          ORACOR            6 0          *OAR (6) IN HH:MM:SS
065300970528     C                   Z-ADD     WN8           G08DAT
065400970528     C                   Z-ADD     *ZEROS        G08INV
065500970528     C                   MOVEL     '0'           G08ERR
065600970528     C                   CALL      'XSRDA8'
065700970528     C                   PARM                    WLBDA8
065800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
065900050519     C* Reperisco la data minima ammessa
066000050519     C                   movel     '$3'          tcod
066100050519     C                   movel(p)  '1'           tkey
066200050519     C     keytab        chain     tabel00f
066300050519     C                   if        %found(tabel00f)
066400050519     C                   movel     tbluni        DS$3
066500050519     c* faccio data+1 e la inverto in gg/mm/aaaa per poterla visualizzarla
066600050519     c* in caso di errore (é la data minima richiedibile a video)
066700050519     c                   move      §$3d10        dataiso
066800050519     c     dataiso       adddur    1:*d          dataiso
066900050519     C                   CLEAR                   WLBDA8
067000050519     C                   move      dataiso       G08INV
067100050519     C                   eval      g08err = '3'
067200050519     C                   CALL      'XSRDA8'
067300050519     C                   PARM                    WLBDA8
067400050519     C                   Z-ADD     G08DAT        DATMIN
067500050519     C                   endif
067600000310     C*
067700000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
067800000310     C                   EXSR      AZZVID
067900970526     C*
068000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
068100970526     C                   EXSR      IMPFUN
068200970526     C*
068300970526     C                   ENDSR
068400970526     C*------------------------------------------------------------------------*
068500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
068600970526     C*------------------------------------------------------------------------*
068700970526     C     FUNGES        BEGSR
068800970526     C*
068900970526     C* PULISCO CAMPI DI LAVORO
069000970526     C                   Z-ADD     0             W                 5 0
069100970526     C*
069200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
069300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
069400970526     C     1             DO        24            I
069500970526     C                   MOVEL     FUD(I)        FUDRIG
069600970526     C*
069700970526     C* PULIZIA FUNZIONI PRECEDENTI
069800970526     C                   MOVEL     *BLANKS       FUV(I)
069900970526     C*
070000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
070100970526     C* (SALTA F24)
070200970526     C     FUDATT        IFEQ      'S'
070300970526     C     FUDFUN        ANDNE     '24'
070400970526     C                   ADD       1             W
070500970526     C                   MOVEL     FUDDES        FUV(W)
070600970526     C                   END
070700970526     C                   END                                                    DO WHILE
070800970526     C*
070900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
071000970526     C* ALTRIMENTI DISATTIVO F24
071100970526     C     1             DO        24            I
071200970526     C                   MOVEL     FUD(I)        FUDRIG
071300970526     C*
071400970526     C     FUDFUN        IFEQ      '24'
071500970526     C     W             IFGT      6
071600970526     C                   MOVEL     'S'           FUDATT
071700970526     C                   ELSE
071800970526     C                   MOVEL     *BLANKS       FUDATT
071900970526     C                   END                                                    W GT 8
072000970526     C                   MOVEL     FUDRIG        FUD(I)
072100970526     C                   END
072200970526     C                   END                                                    DO WHILE
072300970526     C*
072400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
072500970526     C     W             DIV       6             I
072600970526     C                   MVR                     Z
072700970526     C*
072800970526     C     Z             IFNE      0
072900970526     C     I             ADD       1             Z
073000970526     C                   ELSE
073100970526     C                   Z-ADD     I             Z                 5 0
073200970526     C                   END
073300970526     C*
073400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
073500970526     C                   ADD       1             FUNGRU
073600970526     C     FUNGRU        IFGT      Z
073700970526     C     FUNGRU        ORLT      1
073800970526     C                   Z-ADD     1             FUNGRU
073900970526     C                   END
074000970526     C*
074100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
074200970526     C     FUNGRU        IFEQ      1
074300970526     C                   MOVEL     FUV(1)        FUN001
074400970526     C                   MOVEL     FUV(2)        FUN002
074500970526     C                   MOVEL     FUV(3)        FUN003
074600970526     C                   MOVEL     FUV(4)        FUN004
074700970526     C                   MOVEL     FUV(5)        FUN005
074800970526     C                   MOVEL     FUV(6)        FUN006
074900970526     C                   END
075000970526     C*
075100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
075200970526     C     FUNGRU        IFEQ      2
075300970526     C                   MOVEL     FUV(7)        FUN001
075400970526     C                   MOVEL     FUV(8)        FUN002
075500970526     C                   MOVEL     FUV(9)        FUN003
075600970526     C                   MOVEL     FUV(10)       FUN004
075700970526     C                   MOVEL     FUV(11)       FUN005
075800970526     C                   MOVEL     FUV(12)       FUN006
075900970526     C                   END
076000970526     C*
076100970526     C* TERZO GRUPPO DI 6 FUNZIONI
076200970526     C     FUNGRU        IFEQ      3
076300970526     C                   MOVEL     FUV(13)       FUN001
076400970526     C                   MOVEL     FUV(14)       FUN002
076500970526     C                   MOVEL     FUV(15)       FUN003
076600970526     C                   MOVEL     FUV(16)       FUN004
076700970526     C                   MOVEL     FUV(17)       FUN005
076800970526     C                   MOVEL     FUV(18)       FUN006
076900970526     C                   END
077000970526     C*
077100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
077200970526     C     FUNGRU        IFEQ      4
077300970526     C                   MOVEL     FUV(19)       FUN001
077400970526     C                   MOVEL     FUV(20)       FUN002
077500970526     C                   MOVEL     FUV(21)       FUN003
077600970526     C                   MOVEL     FUV(22)       FUN004
077700970526     C                   MOVEL     *BLANKS       FUN005
077800970526     C                   MOVEL     *BLANKS       FUN006
077900970526     C                   END
078000970526     C*
078100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
078200970526     C                   MOVEL     *BLANKS       FUN007
078300970526     C                   MOVEL     *BLANKS       FUN008
078400970526     C*
078500970526     C     1             DO        24            I
078600970526     C                   MOVEL     FUD(I)        FUDRIG
078700970526     C     FUDFUN        IFEQ      '24'
078800970526     C     FUDATT        ANDEQ     'S'
078900970526     C                   MOVEL     FUDDES        FUN008
079000970526     C                   END
079100970526     C                   END                                                    DO
079200970526     C*
079300970526     C                   ENDSR
079400970526     C*------------------------------------------------------------------------*
079500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
079600970526     C*------------------------------------------------------------------------*
079700970526     C     FUNCON        BEGSR
079800970526     C*
079900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080000970526     C                   EXSR      FUNVAR
080100970526     C*
080200970526     C* PULISCO CAMPI DI LAVORO
080300970526     C                   Z-ADD     0             I                 5 0
080400970526     C                   Z-ADD     0             W                 5 0
080500970526     C*
080600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
080700970526     C     FUNKEY        IFNE      *BLANKS
080800970526     C     1             DO        24            I
080900970526     C                   MOVEL     FUD(I)        FUDRIG
081000970526     C*
081100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081200970526     C     FUNKEY        IFEQ      FUDFUN
081300970526     C     FUDATT        ANDEQ     'S'
081400970526     C                   Z-ADD     I             W
081500970526     C                   END
081600970526     C                   END                                                    DO WHILE
081700970526     C*
081800970526     C* FUNZIONE NON ATTIVA (ERRORE)
081900970526     C     W             IFEQ      0
082000970526     C*
082100970526     C* EMISSIONE MESSAGGIO DI ERRORE
082200970526     C                   MOVEL     MSG(1)        DSMSMS
082300970526     C*
082400970526     C                   END                                                    W=0
082500970526     C                   END                                                    FUNKEY NE *BLANKS
082600970526     C*
082700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
082800970526     C     W             IFNE      0
082900970526     C     FUNKEY        OREQ      *BLANKS
083000970526     C     FUNKEY        IFEQ      *BLANKS
083100970526     C                   EXSR      ENTGES
083200970526     C                   END
083300970526     C     FUNKEY        IFEQ      '03'
083400970526     C                   EXSR      F03GES
083500970526     C                   END
083600970526     C     FUNKEY        IFEQ      '06'
083700970526     C                   EXSR      F06GES
083800970526     C                   END
083900970526     C     FUNKEY        IFEQ      '12'
084000970526     C                   EXSR      F12GES
084100970526     C                   END
084200970526     C     FUNKEY        IFEQ      '24'
084300970526     C                   EXSR      F24GES
084400970526     C                   END
084500970526     C                   END
084600970526     C*
084700970526     C                   ENDSR
084800970526     C*------------------------------------------------------------------------*
084900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
085000970526     C*------------------------------------------------------------------------*
085100970526     C     FUNVAR        BEGSR
085200970526     C*
085300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
085400970526     C*
085500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
085600970526     C     KEY           IFEQ      ENTER
085700970526     C                   MOVEL     *BLANKS       FUNKEY
085800970526     C                   END
085900970526     C     KEY           IFEQ      F1
086000970526     C                   MOVEL     '01'          FUNKEY
086100970526     C                   END
086200970526     C     KEY           IFEQ      F2
086300970526     C                   MOVEL     '02'          FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      F3
086600970526     C                   MOVEL     '03'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      F4
086900970526     C                   MOVEL     '04'          FUNKEY
087000970526     C                   END
087100970526     C     KEY           IFEQ      F5
087200970526     C                   MOVEL     '05'          FUNKEY
087300970526     C                   END
087400970526     C     KEY           IFEQ      F6
087500970526     C                   MOVEL     '06'          FUNKEY
087600970526     C                   END
087700970526     C     KEY           IFEQ      F7
087800970526     C                   MOVEL     '07'          FUNKEY
087900970526     C                   END
088000970526     C     KEY           IFEQ      F8
088100970526     C                   MOVEL     '08'          FUNKEY
088200970526     C                   END
088300970526     C     KEY           IFEQ      F9
088400970526     C                   MOVEL     '09'          FUNKEY
088500970526     C                   END
088600970526     C     KEY           IFEQ      F10
088700970526     C                   MOVEL     '10'          FUNKEY
088800970526     C                   END
088900970526     C     KEY           IFEQ      F11
089000970526     C                   MOVEL     '11'          FUNKEY
089100970526     C                   END
089200970526     C     KEY           IFEQ      F12
089300970526     C                   MOVEL     '12'          FUNKEY
089400970526     C                   END
089500970526     C     KEY           IFEQ      F13
089600970526     C                   MOVEL     '13'          FUNKEY
089700970526     C                   END
089800970526     C     KEY           IFEQ      F14
089900970526     C                   MOVEL     '14'          FUNKEY
090000970526     C                   END
090100970526     C     KEY           IFEQ      F15
090200970526     C                   MOVEL     '15'          FUNKEY
090300970526     C                   END
090400970526     C     KEY           IFEQ      F16
090500970526     C                   MOVEL     '16'          FUNKEY
090600970526     C                   END
090700970526     C     KEY           IFEQ      F17
090800970526     C                   MOVEL     '17'          FUNKEY
090900970526     C                   END
091000970526     C     KEY           IFEQ      F18
091100970526     C                   MOVEL     '18'          FUNKEY
091200970526     C                   END
091300970526     C     KEY           IFEQ      F19
091400970526     C                   MOVEL     '19'          FUNKEY
091500970526     C                   END
091600970526     C     KEY           IFEQ      F20
091700970526     C                   MOVEL     '20'          FUNKEY
091800970526     C                   END
091900970526     C     KEY           IFEQ      F21
092000970526     C                   MOVEL     '21'          FUNKEY
092100970526     C                   END
092200970526     C     KEY           IFEQ      F22
092300970526     C                   MOVEL     '22'          FUNKEY
092400970526     C                   END
092500970526     C     KEY           IFEQ      F23
092600970526     C                   MOVEL     '23'          FUNKEY
092700970526     C                   END
092800970526     C     KEY           IFEQ      F24
092900970526     C                   MOVEL     '24'          FUNKEY
093000970526     C                   END
093100970526     C     KEY           IFEQ      ROLLUP
093200970526     C                   MOVEL     'RU'          FUNKEY
093300970526     C                   END
093400970526     C     KEY           IFEQ      HELP
093500970526     C                   MOVEL     'HP'          FUNKEY
093600970526     C                   END
093700970526     C                   ENDSR
093800970526     C*------------------------------------------------------------------------*
093900970526     C* NOTE COSTRUZIONE SCHIERE
094000970526     C*------------------------------------------------------------------------*
094100970526     C*
094200970526     C* -MSG-MESSAGGI
094300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
094400970526     C* (NON TOCCARE MSG,1 E MSG,2)
094500970526     C*
094600970526     C* -CMD-COMANDI
094700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
094800970526     C*
094900970526     C* -OPD-OPZIONI
095000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
095100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
095200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
095300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
095400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
095500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
095600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
095700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
095800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
095900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
096000970526     C*
096100970526     C* -FUD-FUNZIONI
096200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
096300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
096400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
096500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
096600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
096700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
096800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
096900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
097000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
097100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
097200970526     O*------------------------------------------------------------------------*
097300970526** MSG - MESSAGGI
097400040628TIS7W4R- Tasto funzionale non gestito                                          1
097500040628TIS7W4R-                                                                       2
097600040628TIS7W4R-                                                                       3
097700040628TIS7W4R-                                                                       4
097800040628TIS7W4R-                                                                       5
097900040628TIS7W4R-                                                                       6
098000040628TIS7W4R-                                                                       7
098100040628TIS7W4R-                                                                       8
098200040628TIS7W4R-                                                                       9
098300040628TIS7W4R-                                                                       10
098400040628TIS7W4R-                                                                       11
098500040628TIS7W4R-                                                                       12
098600040628TIS7W4R-                                                                       13
098700040628TIS7W4R- Data errata                                                           14
098800040628TIS7W4R- La data inizio deve essere maggiore della data fine                   15
098900040628TIS7W4R- Cliente inserito inesistente.                                         16
099000040628TIS7W4R- Coda di stampa inesistente.                                           17
099100050518TIS7W4R- Tipo legame inesistente o mancante.                                   18
099200050518TIS7W4R- Cliente mancante.                                                     19
099300050519TIS7W4R- Immettere una data più recente. Data minima ammessa:xx/xx/xxxx        20
099400040628TIS7W4R- Scelta obbligatoria.                                                  21
099500040628TIS7W4R-                                                                       22
099600040628TIS7W4R-                                                                       23
099700040628TIS7W4R-                                                                       24
099800970526** FUD - FUNZIONI
099900970526                   -        (S/N)=N                        funz.01¬
100000970526                   -        (S/N)=N                        funz.02¬
100100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
100200970526                   -        (S/N)=N                        funz.04¬
100300970526                   -        (S/N)=N                        funz.05¬
100400991214F06=Conferma       -        (S/N)=S                        funz.06¬
100500970527                   -        (S/N)=N                        funz.07¬
100600970527                   -        (S/N)=N                        funz.08¬
100700970527                   -        (S/N)=N                        funz.09¬
100800970527                   -        (S/N)=N                        funz.10¬
100900970526                   -        (S/N)=N                        funz.11¬
101000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
101100970526                   -        (S/N)=N                        funz.13¬
101200970526                   -        (S/N)=N                        funz.14¬
101300970526                   -        (S/N)=N                        funz.15¬
101400970526                   -        (S/N)=N                        funz.16¬
101500970526                   -        (S/N)=N                        funz.17¬
101600970526                   -        (S/N)=N                        funz.18¬
101700970526                   -        (S/N)=N                        funz.19¬
101800970526                   -        (S/N)=N                        funz.20¬
101900970526                   -        (S/N)=N                        funz.21¬
102000970526                   -        (S/N)=N                        funz.22¬
102100970526                   -        (S/N)=N                        funz.23¬
102200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
