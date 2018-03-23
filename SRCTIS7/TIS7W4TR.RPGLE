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
011600080213     D  PRMVGD                        1
011700080213     D  PRMCLIVAS                     8
011800020923     D*-------------------
011900020923     D* DS REPERIMENTO DATI UTENTE
012000020923     D*-------------------
012100020923     D TIBS34DS      E DS                                                       *Profili utente
012200020923     D DDATIUTE      E DS                                                       *Dati utente
012300020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012400970526     C*------------------------------------------------------------------------*
012500000303     C* MAIN LINES
012600970526     C*------------------------------------------------------------------------*
012700970526     C*
012800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012900991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013200970526     C*
013300970526     C* EMETTE FORMATO VIDEO
013400970526     C                   EXSR      BUTFMR
013500970526     C*
013600970526     C* GESTIONE TASTI FUNZIONALI
013700970526     C                   EXSR      FUNCON
013800970526E   2C                   ENDDO
013900970526     C*
014000970526     C* FINE PGM
014100970526     C                   SETON                                        LR
014200050518     c*
014300050518     C                   CLEAR                   tibs02ds
014400050518     C                   MOVEL     'C'           T02TLA
014500050518     C                   CALL      'TIBS02R'
014600050518     C                   PARM                    KPJBA
014700050518     C                   PARM                    tibs02ds
014800970526     C*------------------------------------------------------------------------*
014900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     BUTFMR        BEGSR
015200970526     C*
015300970526     C* EMETTE LA TESTATA
015400040628     C                   WRITE     S7W4D01
015500970526     C*
015600970526     C* SPEDISCE EVENTUALE MESSAGGIO
015700970526     C                   EXSR      SNDMSG
015800970526     C*
015900970526     C* EMETTE LE FUNZIONI
016000040628     C                   WRITE     S7W4D02
016100970526     C*
016200970526     C* EMETTE IL VIDEO
016300040628     C                   EXFMT     S7W4D03
016400970526     C*
016500991214     C* REIMPOSTA FLAG DI ERRORE
016600991214     C                   MOVEL     '0'           ERRL00
016700970526     C*
016800970526     C* REIMPOSTA INDICATORI DI ERRORE
016900050518     C                   SETOFF                                       545556
017000080606     C                   SETOFF                                       4557
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     SNDMSG        BEGSR
017700970526     C*
017800970526     C* PULISCE IL SFILE MESSAGGI
017900970526     C                   MOVEL     '1'           *IN40
018000040628     C                   WRITE     S7W4DMC
018100970526     C                   MOVEL     '0'           *IN40
018200970526     C*
018300970526     C* VISUALIZZA IL MESSAGGIO
018400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018500970526     C                   CALL      'XMSG'        XMSGPA
018600020611     C     DSMSER        IFEQ      *ZEROS
018700970526     C                   MOVEL     DSMSKE        MSGKEY
018800020611     C                   ENDIF
018900970526     C                   Z-ADD     1             MSGULT
019000040628     C                   WRITE     S7W4DMS
019100040628     C                   WRITE     S7W4DMC
019200970526     C*
019300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019400970526     C                   MOVEL     *BLANKS       DSMSMS
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* ENTGES - GESTIONE ENTER -> INVIO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     ENTGES        BEGSR
020100970526     C*
020200970526     C* CONTROLLA I CAMPI IMMESSI
020300970526     C                   EXSR      DECODI
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F03GES - GESTIONE F03 -> FINE
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F03GES        BEGSR
021000970526     C*
021100970526     C* IMPOSTA "PREMUTO F03"
021200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600991214     C* F06GES - GESTIONE F06 -> CONFERMA
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F06GES        BEGSR
021900970526     C*
022000991214     C* CONTROLLA I CAMPI IMMESSI
022100991214     C                   EXSR      DECODI
022200020611     C*
022300991214     C* ESEGUE ROUTINE DI CONFERMA
022400020611     C     ERRL00        IFEQ      '0'
022500991214     C                   EXSR      ESEGUI
022600030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
022700991214     C                   ENDIF
022800970526     C*
022900991214     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* F12GES - GESTIONE F12 -> RITORNO
023200970526     C*------------------------------------------------------------------------*
023300970526     C     F12GES        BEGSR
023400970526     C*
023500970526     C* IMPOSTA "PREMUTO F12"
023600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F24GES        BEGSR
024300970526     C*
024400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024500970526     C*
024600970526     C                   ENDSR
024700991214     C*------------------------------------------------------------------------*
024800991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
024900991214     C*------------------------------------------------------------------------*
025000040628     C     PASPAR        BEGSR
025100991214     C*
025200040628     C                   CLEAR                   TIS1W2IDS
025300040628     C*
025400040628     C                   MOVE(P)   VIDKSC        wVIDKSCnum        8 0
025500040628     C                   EVAL      RQSCID = VIDSGI
025600040628     C                   MOVE      wVIDKSCnum    KSC
025700040628     C                   MOVE      VIDDAI        wVIDDAIAlfa       8
025800040628     C                   EVAL      DIN = %subst(wVIDDAIAlfa:1:2)+'/'+
025900040628     C                                   %subst(wVIDDAIAlfa:3:2)+'/'+
026000040628     C                                   %subst(wVIDDAIAlfa:5:4)
026100040628     C                   MOVE      VIDDAF        wVIDDAFAlfa       8
026200040628     C                   EVAL      DFI = %subst(wVIDDAFAlfa:1:2)+'/'+
026300040628     C                                   %subst(wVIDDAFAlfa:3:2)+'/'+
026400040628     C                                   %subst(wVIDDAFAlfa:5:4)
026500040628     C                   EVAL      LNA = VIDLNA
026600040628     C                   EVAL      CKC = VIDCKC
026700040628     C                   EVAL      CKG = VIDCKG
026800040628     C                   EVAL      CKA = VIDCKA
026900040628     C                   EVAL      CKM = VIDCKM
027000050210     C                   EVAL      CKP = VIDCKP
027100050210     C                   EVAL      CKT = VIDCKT
027200050210     C                   EVAL      CKL = VIDCKL
027300080606     C                   EVAL      SERVIZIO_C = VIDTSPC
027400080606     C                   EVAL      SERVIZIO_D = VIDTSPD
027500080606     C                   EVAL      SERVIZIO_E = VIDTSPE
027600091111     C                   EVAL      SERVIZIO_H = VIDTSPH
027700991214     C*
027800040628     C* Edito la data e ora corrente
027900040628     C                   MOVEL     WN8           wDATCORAlfa       8
028000040628     C                   MOVEL     ORACOR        wORACORAlfa       6
028100040628     C                   EVAL      PRMDAT = %subst(wDATCORAlfa:1:2)+'/'+
028200040628     C                                      %subst(wDATCORAlfa:3:2)+'/'+
028300040628     C                                      %subst(wDATCORAlfa:5:4)
028400040628     C                   EVAL      PRMORA = %subst(wORACORAlfa:1:2)+':'+
028500040628     C                                      %subst(wORACORAlfa:3:2)+':'+
028600040628     C                                      %subst(wORACORAlfa:5:2)
028700040628     C*
028800040628     C                   EVAL      PRMTIS1W2IDS = TIS1W2IDS
028900050518     c*
029000080213     c                   eval      prmsuf     = vidsuf
029100080213     c                   eval      prmtlg     = vidtlg
029200080213     c                   eval      prmfile    = vidpf
029300080213     c                   eval      prmvgd     = *blanks
029400080213     c                   eval      prmclivas  = *blanks
029500040628     C*
029600991214     C                   ENDSR
029700970526     C*------------------------------------------------------------------------*
029800991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
029900970526     C*------------------------------------------------------------------------*
030000991214     C     ESEGUI        BEGSR
030100991214     C*
030200991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
030300040628     C                   MOVEL     'S7W5'        KCOAZ
030400991214     C                   EXSR      PASPAR
030500040628     C                   MOVEL     PARAM         KPJBU
030600140328     C***                CALL      'BCH09'
030700140328     C                   CALL      'BCH10'
030800071024 MOM C***                CALL      'TIS7W5C'
030900991214     C                   PARM                    KPJBA
031000991214     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031400970526     C*------------------------------------------------------------------------*
031500970526     C     DECODI        BEGSR
031600000306     C*
031700050518     C                   SETOFF                                       545545
031800080606     C                   SETOFF                                       5657
031900050617     C                   SETON                                        46
032000000306     C*
032100050518     C*--------------------
032200050518     C* CLIENTE SINGOLO/UNIFICANTE
032300050518     c* Se scelto U=Unificante attivo ed imposto campo tipo legame
032400050518     C*--------------------
032500050518    1c     errl00        ifeq      '0'
032600050518    2c                   if        vidsuf = 'U' and *in44 = *on
032700050518     c                   eval      vidtlg = 'ST'
032800050518     c                   setoff                                       44
032900050518     c                   eval      errl00 = '1'
033000050518   x2c                   else
033100050518    3c                   if        vidsuf = 'S' and *in44 = *off
033200050518     c                   seton                                        44
033300050518     c                   eval      vidtlg = *blanks
033400050518     c                   eval      errl00 = '1'
033500050518    3c                   endif
033600050518    2c                   endif
033700050518    1c                   endif
033800050518     C*--------------------
033900050518     C* TIPO LEGAME
034000050518     C*--------------------
034100050518    1c                   if        *in44 = *off
034200050518    2c     errl00        ifeq      '0'
034300050518     c* ricerca
034400050518     C     '?'           SCAN      VIDtlg                                 30
034500050518    3C     *IN30         IFEQ      *ON
034600050518     c                   Clear                   tibs02ds
034700050518     c                   Eval      T02Mod = 'R'
034800050518     c                   Eval      T02Sif = Knsif
034900050518     c                   Eval      T02Cod = 'TLE'
035000050518     c                   Call      'TIBS02R'
035100050518     c                   Parm                    Kpjba
035200050518     c                   Parm                    tibs02ds
035300050518     c                   Movel     T02Ke1        VidTlg
035400050518     c                   Eval      errl00 = '1'
035500050518    3c                   endif
035600050518     c* errore se tipo legame non immesso
035700050518    3c     errl00        ifeq      '0'
035800050518     c     vidtlg        andeq     *blanks
035900050518     c                   seton                                        45
036000050518     C                   MOVEL     MSG(18)       DSMSMS
036100050518     c                   movel     '1'           errl00
036200050518    3c                   endif
036300050518     c* controllo esistenza in tabella
036400050518    3c     errl00        ifeq      '0'
036500050518     C                   CLEAR                   tibs02ds
036600050518     c                   clear                   dtle
036700050518     c                   clear                   viddtl
036800050518     C                   MOVEL     'C'           T02MOD
036900050518     C                   MOVEL     KNSIF         T02SIF
037000050518     C                   MOVEL     'TLE'         T02COD
037100050518     c                   movel(P)  vidtlg        t02ke1
037200050518     C                   CALL      'TIBS02R'
037300050518     C                   PARM                    KPJBA
037400050518     C                   PARM                    tibs02ds
037500050518    4C                   IF        T02ERR = *BLANKS
037600050518     C                   MOVEL     T02uni        dtle
037700050518     c                   else
037800050518     C                   SETON                                        45
037900050518     C                   MOVEL     MSG(18)       DSMSMS
038000050518     c                   movel     '1'           errl00
038100050518    4C                   ENDIF
038200050518     c                   movel     §tledesc      viddtl
038300050518    3c                   endif
038400050518    2c                   endif
038500050518    1c                   endif
038600050518     c
038700991215     C*--------------------
038800000306     C* CLIENTI
038900991215     C*--------------------
039000991215     C     ERRL00        IFEQ      '0'
039100991216     C     '?'           SCAN      VIDKSC                                 30
039200991215     C     *IN30         IFEQ      *ON
039300991215     C                   CLEAR                   DSXC
039400020923     C                   Z-ADD     DUTKCI        DXCCAP
039500991215     C                   CALL      'XCLIR'
039600991215     C                   PARM                    DSXC
039700050519     C                   PARM                    VIDTLG
039800991215     C     DXCERR        ifne      '1'
039900991216     C*-------------
040000040628     C                   MOVEA     DXCKSC        SKKSA                      ALF
040100991216DO  2C     1             DO        30            J                 3 0
040200040628     C                   MOVE      SKKSA(J)      DSKSA                      F
040300040628     C                   Z-ADD     DSKSN         SKKSC(J)                   M
040400991216E   2C                   ENDDO
040500991216     C*-------------
040600040628     C                   MOVE      SKKSC(1)      VIDKSC
040700991215     C                   ENDIF
040800991215     C                   ENDIF
040900991216     C     VIDKSC        IFNE      *zeros
041000991216     C     VIDKSC        ANDNE     *blanks
041100020923     C                   Z-ADD     DUTKCI        kclkcc
041200020611     C                   MOVEL     VIDKSC        kclksc
041300020611     C     keycli        chain     cnaco00f                           99
041400991215     C     *IN99         ifeq      *off
041500991216     C                   MOVEL     acorag        VIDRAG
041600020611     C                   endif
041700050518     c                   else
041800050518     c* errore se non immesso
041900050518     c                   clear                   vidrag
042000050518     c                   seton                                        56
042100050518     C                   MOVEL     MSG(19)       DSMSMS
042200050518     c                   Eval      errl00 = '1'
042300000306     C                   ENDIF
042400050518     c* se richiesto cliente unificante e il cliente immesso è figlio
042500050518     c* propongo in automatico il relativo codice padre
042600050518     C     ERRL00        IFEQ      '0'
042700050518     c     vidsuf        andeq     'U'
042800050518     c                   clear                   tibs10ds
042900050518     c                   eval      d10tle = vidtlg
043000050518     c                   eval      d10paf = 'P'
043100050518     c                   move      vidksc        d10cod
043200050518     c                   z-add     datcor        d10drf
043300050518     c                   call      'TIBS10R'
043400050518     c                   Parm                    Tibs10ds
043500050518     c                   If        d10err =  *Blanks
043600050518     c                   move      d10cop        vidksc
043700050518     c                   endif
043800050518     c                   endif
043900050518     c
044000000306     C                   ENDIF
044100991015     C*--------------------
044200020611     C* DATA INIZIO
044300991015     C*--------------------
044400020611     C     *LIKE         DEFINE    VIDDAI        DAIVID
044500020611     C     ERRL00        IFEQ      '0'
044600020611     C                   Z-ADD     VIDDAI        WDA8
044700020611     C                   Z-ADD     *ZEROS        DA8W
044800991015     C                   EXSR      CHKDA8
044900020611     C                   Z-ADD     WDA8          VIDDAI
045000020611     C                   Z-ADD     DA8W          DAIVID
045100020611     C     ERRL00        IFEQ      '1'
045200991214     C                   SETON                                        54
045300020611     C                   MOVEL     MSG(14)       DSMSMS
045400050519     c                   else
045500050519     c* la data inizio deve essere > della data spedizione più alta presente
045600050519     c* nel titasp (tale data è memorizzata in tabella $3 campo §$3D10)
045700050519     c* In sostanza vanno bene solo le date spedizioni presenti nel titas10
045800050519     c* o nel titas0. Non si accettano date spedizioni più vecchie in
045900050519     c* quanto il batch estrae i dati solo dal titas10 e dal titas0
046000080110     C                   if        KNMUS <> 'EDPFG'
046100050519     c                   if        daivid <= §$3d10
046200050519     C                   SETON                                        54
046300050519     c* edito la data da visualizzare nel messaggio
046400050519     c                   eval      %subst(msg(20):62:10) =
046500050519     c                             %editw(datmin:'  /  /    ')
046600050519     C                   MOVEL     MSG(20)       DSMSMS
046700050519     c                   eval      errl00 = '1'
046800050519     c                   endif
046900080110     C                   ENDIF
047000020611     C                   ENDIF
047100020611     C                   ENDIF
047200970528     C*--------------------
047300020611     C* DATA FINE
047400970528     C*--------------------
047500020611     C     *LIKE         DEFINE    VIDDAF        DAFVID
047600020611     C     ERRL00        IFEQ      '0'
047700020611     C                   Z-ADD     VIDDAF        WDA8
047800020611     C                   Z-ADD     *ZEROS        DA8W
047900970528     C                   EXSR      CHKDA8
048000020611     C                   Z-ADD     WDA8          VIDDAF
048100020611     C                   Z-ADD     DA8W          DAFVID
048200020611     C     ERRL00        IFEQ      '1'
048300991214     C                   SETON                                        55
048400970528     C                   MOVEL     MSG(14)       DSMSMS
048500020611     C                   ENDIF
048600020611     C                   ENDIF
048700020611     C*
048800970528     C* SCADENZA > DECORRENZA
048900020611     C     ERRL00        IFEQ      '0'
049000020611     C     DAFVID        IFLT      DAIVID
049100020611     C                   SETON                                        5455
049200970528     C                   MOVEL     MSG(15)       DSMSMS
049300970528     C                   MOVEL     '1'           ERRL00
049400020611     C                   ENDIF
049500020611     C                   ENDIF
049600050617     C*
049700050617     C* VERIFICA ABILITAZIONE CLIENTE X OUTPUT STATISTICA SU FILE
049800050617     C     ERRL00        IFEQ      '0'
049900050617     C     VIDKSC        IFNE      *ZEROS
050000050617     C                   EVAL      tbeCOD = 'AVC'
050100050617     C                   EVAL      tbeKE1 = VIDKSC
050200050617     C     keytbe1_P     CHAIN     TNTBE01L
050300050617     C                   IF        %found(TNTBE01L)
050400050617     C                   EVAL      DAVC = tbeUNI
050500050617     C                   IF        §AVCS7W5 = 'S'
050600050617     C                   SETOFF                                       46
050700050617     C                   ENDIF
050800050617     C                   ENDIF
050900050617     C                   ENDIF
051000050617     C                   ENDIF
051100050617     C                   IF        *IN46 = *ON
051200050617     C                   EVAL      VIDPF = *BLANKS
051300050617     C                   ENDIF
051400080606     C*--------------------
051500080606     C* TIPO SERVIZIO
051600080606     C*--------------------
051700080606     C     ERRL00        IFEQ      '0'
051800080606     C                   IF        VIDTSPC = *blanks AND
051900080606     C                             VIDTSPD = *blanks AND
052000091111     C                             VIDTSPE = *blanks AND
052100091111     C                             VIDTSPH = *blanks
052200080606     C                   SETON                                        57
052300080606     C                   MOVEL     MSG(22)       DSMSMS
052400080606     c                   Eval      errl00 = '1'
052500080606     C                   ENDIF
052600080606     C                   ENDIF
052700000306     C*
052800970528     C                   ENDSR
052900970528     C*------------------------------------------------------------------------*
053000970528     C* CHKDA8 - CONTROLLA LA DATA (8)
053100970528     C*------------------------------------------------------------------------*
053200970528     C     CHKDA8        BEGSR
053300970528     C*
053400020611     C     *LIKE         DEFINE    VIDDAI        WDA8
053500020611     C     *LIKE         DEFINE    VIDDAF        DA8W
053600970528     C*
053700970528     C                   Z-ADD     WDA8          G08DAT
053800970528     C                   Z-ADD     *ZEROS        G08INV
053900970528     C                   MOVEL     '0'           G08ERR
054000970528     C                   CALL      'XSRDA8'
054100970528     C                   PARM                    WLBDA8
054200970528     C                   Z-ADD     G08DAT        WDA8
054300970528     C                   Z-ADD     G08INV        DA8W
054400020611     C     G08ERR        IFEQ      '1'
054500970528     C                   MOVEL     '1'           ERRL00
054600020611     C                   ENDIF
054700970528     C*
054800970528     C                   ENDSR
054900970526     C*------------------------------------------------------------------------*
055000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
055100970526     C*------------------------------------------------------------------------*
055200970526     C     AZZVID        BEGSR
055300970526     C* CAMPI
055400050518     c                   movel     'S'           vidsuf
055500050518     c                   clear                   vidtlg
055600050518     c                   seton                                        44
055700020611     C                   Z-ADD     *ZEROS        VIDDAI
055800020611     C                   Z-ADD     *ZEROS        VIDDAF
055900040628     C                   Z-ADD     999           VIDLNA
056000040628     C                   MOVEL     'S'           VIDCKC
056100040628     C                   MOVEL     'S'           VIDCKM
056200040628     C                   MOVEL     'S'           VIDCKG
056300040628     C                   MOVEL     'S'           VIDCKA
056400050210     C                   MOVEL     'S'           VIDCKP
056500050210     C                   MOVEL     'S'           VIDCKT
056600050210     C                   MOVEL     'S'           VIDCKL
056700080606     C                   MOVEL     'C'           VIDTSPC
056800080606     C                   MOVEL     'D'           VIDTSPD
056900080606     C                   MOVEL     'E'           VIDTSPE
057000100114     C                   MOVEL     'H'           VIDTSPH
057100040628     C
057200970526     C* DECODIFICHE
057300991216     C                   MOVEL     *BLANKS       VIDRAG
057400050518     C                   MOVEL     *BLANKS       VIDDTL
057500970526     C*
057600970526     C                   ENDSR
057700970526     C*------------------------------------------------------------------------*
057800020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
057900970526     C*------------------------------------------------------------------------*
058000970526     C     IMPFUN        BEGSR
058100970526     C*
058200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
058300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
058400970526     C                   EXSR      FUNGES
058500970526     C*
058600970526     C                   ENDSR
058700020923     C*--------------------------------------------------------------------------------------------*
058800020923     C* REPERISCE I DATI UTENTE
058900020923     C*--------------------------------------------------------------------------------------------*
059000020923     C     REPDATIUTE    BEGSR
059100020923     C*
059200020923     C* INIZIALIZZA VARIABILI DI WRK
059300020923     C                   CLEAR                   TIBS34DS
059400020923     C                   CLEAR                   AZUTEDS
059500020923     C                   CLEAR                   DDATIUTE
059600020923     C*
059700020923     C     *DTAARA       DEFINE    §azute        azuteds
059800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
059900020923     C                   IN(E)     *DTAARA
060000020923if  1C                   IF        %Error
060100020923     c                   EVAL      I34Tla = 'L'
060200020923     C                   CALL      'TIBS34R'
060300020923     C                   PARM                    Tibs34Ds
060400020923     C                   IN        *DTAARA
060500020923e   1C                   ENDIF
060600020923     C*
060700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
060800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
060900020923     C*
061000020923     C                   ENDSR
061100970526     C*------------------------------------------------------------------------*
061200970526     C* *INZSR - OPERAZIONI INIZIALI
061300970526     C*------------------------------------------------------------------------*
061400970526     C     *INZSR        BEGSR
061500991217     C*
061600991217     C     *ENTRY        PLIST
061700991217     C                   PARM                    KPJBA
061800020923     C*
061900020923     C* REPERISCE I DATI DELL'UTENTE
062000020923     C                   EXSR      REPDATIUTE
062100050617     C*
062200050617     C* IMPOSTO SUBITO ALCUNI INDICATORI D WRK
062300050617     C                   SETON                                        46
062400050503     C*
062500050503     C* VERIFICO L'UTENTE CHE EFFETTUA LA RICHIESTA, SE NN 'EDP*' => INIBISCO ALCUNI CAMPI A VIDEO
062600050503     C                   IF        %subst(%trim(KNMUS):1:3) <> 'EDP'
062700050503     C                   SETON                                        50
062800050503     C                   ELSE
062900050503     C                   SETOFF                                       50
063000050503     C                   ENDIF
063100991215     C*
063200000306     C* LETTURA cnaco31c/cnaco00f
063300991215     C     keycli        KLIST                                                  *di cnaco/ind/clp
063400991215     C                   KFLD                    kclkut            1 0          -utente
063500991215     C                   KFLD                    kclkcc            4 0          -capoconto
063600991215     C                   KFLD                    kclksc            7 0          -codice
063700991216     C                   z-add     1             kclkut
063800050519     C* Definizione chiavi - TABEL00F
063900050519     C     keytab        KLIST
064000050519     C                   KFLD                    tute
064100050519     C                   KFLD                    tcod
064200050519     C                   KFLD                    tkey
064300050617     C* Definizione chiavi - TNTBE01L
064400050617     C     keytbe1_P     KLIST
064500050617     C                   KFLD                    tbeCOD
064600050617     C                   KFLD                    tbeKE1
064700970526     C*
064800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
064900970526     C     XMSGPA        PLIST
065000970526     C                   PARM                    DSMSMS
065100970526     C                   PARM                    DSMSPG
065200970526     C                   PARM                    DSMSKE
065300970526     C                   PARM                    DSMSER
065400970526     C*
065500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
065600970526     C                   CALL      'XKEY'
065700970526     C                   PARM                    DSKEY
065800970526     C*---
065900970526     C* VARIABILI NON RIFERITE AL DATA BASE
066000970526     C*---
066100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
066200991214     C                   MOVEL     *BLANKS       DEPOP1            3
066300970526     C*---
066400970526     C* VARIABILI DI CONTROLLO
066500970526     C*---
066600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
066700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
066800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
066900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
067000970526     C*
067100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
067200040628     C                   MOVEL     'TIS7W4R'     DSMSPG
067300040628     C                   MOVEL     'TIS7W4R'     MSGQUE
067400970526     C                   MOVEL     *BLANKS       MSGKEY
067500970526     C                   MOVEL     *BLANKS       DSMSMS
067600970528     C*
067700970528     C* CALCOLA LA DATA CORRENTE
067800970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
067900970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
068000040628     C                   MOVEL     WN14          ORACOR            6 0          *OAR (6) IN HH:MM:SS
068100970528     C                   Z-ADD     WN8           G08DAT
068200970528     C                   Z-ADD     *ZEROS        G08INV
068300970528     C                   MOVEL     '0'           G08ERR
068400970528     C                   CALL      'XSRDA8'
068500970528     C                   PARM                    WLBDA8
068600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
068700050519     C* Reperisco la data minima ammessa
068800050519     C                   movel     '$3'          tcod
068900050519     C                   movel(p)  '1'           tkey
069000050519     C     keytab        chain     tabel00f
069100050519     C                   if        %found(tabel00f)
069200050519     C                   movel     tbluni        DS$3
069300050519     c* faccio data+1 e la inverto in gg/mm/aaaa per poterla visualizzarla
069400050519     c* in caso di errore (é la data minima richiedibile a video)
069500050519     c                   move      §$3d10        dataiso
069600050519     c     dataiso       adddur    1:*d          dataiso
069700050519     C                   CLEAR                   WLBDA8
069800050519     C                   move      dataiso       G08INV
069900050519     C                   eval      g08err = '3'
070000050519     C                   CALL      'XSRDA8'
070100050519     C                   PARM                    WLBDA8
070200050519     C                   Z-ADD     G08DAT        DATMIN
070300050519     C                   endif
070400000310     C*
070500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
070600000310     C                   EXSR      AZZVID
070700970526     C*
070800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
070900970526     C                   EXSR      IMPFUN
071000970526     C*
071100970526     C                   ENDSR
071200970526     C*------------------------------------------------------------------------*
071300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
071400970526     C*------------------------------------------------------------------------*
071500970526     C     FUNGES        BEGSR
071600970526     C*
071700970526     C* PULISCO CAMPI DI LAVORO
071800970526     C                   Z-ADD     0             W                 5 0
071900970526     C*
072000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
072100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
072200970526     C     1             DO        24            I
072300970526     C                   MOVEL     FUD(I)        FUDRIG
072400970526     C*
072500970526     C* PULIZIA FUNZIONI PRECEDENTI
072600970526     C                   MOVEL     *BLANKS       FUV(I)
072700970526     C*
072800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
072900970526     C* (SALTA F24)
073000970526     C     FUDATT        IFEQ      'S'
073100970526     C     FUDFUN        ANDNE     '24'
073200970526     C                   ADD       1             W
073300970526     C                   MOVEL     FUDDES        FUV(W)
073400970526     C                   END
073500970526     C                   END                                                    DO WHILE
073600970526     C*
073700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
073800970526     C* ALTRIMENTI DISATTIVO F24
073900970526     C     1             DO        24            I
074000970526     C                   MOVEL     FUD(I)        FUDRIG
074100970526     C*
074200970526     C     FUDFUN        IFEQ      '24'
074300970526     C     W             IFGT      6
074400970526     C                   MOVEL     'S'           FUDATT
074500970526     C                   ELSE
074600970526     C                   MOVEL     *BLANKS       FUDATT
074700970526     C                   END                                                    W GT 8
074800970526     C                   MOVEL     FUDRIG        FUD(I)
074900970526     C                   END
075000970526     C                   END                                                    DO WHILE
075100970526     C*
075200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
075300970526     C     W             DIV       6             I
075400970526     C                   MVR                     Z
075500970526     C*
075600970526     C     Z             IFNE      0
075700970526     C     I             ADD       1             Z
075800970526     C                   ELSE
075900970526     C                   Z-ADD     I             Z                 5 0
076000970526     C                   END
076100970526     C*
076200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
076300970526     C                   ADD       1             FUNGRU
076400970526     C     FUNGRU        IFGT      Z
076500970526     C     FUNGRU        ORLT      1
076600970526     C                   Z-ADD     1             FUNGRU
076700970526     C                   END
076800970526     C*
076900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
077000970526     C     FUNGRU        IFEQ      1
077100970526     C                   MOVEL     FUV(1)        FUN001
077200970526     C                   MOVEL     FUV(2)        FUN002
077300970526     C                   MOVEL     FUV(3)        FUN003
077400970526     C                   MOVEL     FUV(4)        FUN004
077500970526     C                   MOVEL     FUV(5)        FUN005
077600970526     C                   MOVEL     FUV(6)        FUN006
077700970526     C                   END
077800970526     C*
077900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
078000970526     C     FUNGRU        IFEQ      2
078100970526     C                   MOVEL     FUV(7)        FUN001
078200970526     C                   MOVEL     FUV(8)        FUN002
078300970526     C                   MOVEL     FUV(9)        FUN003
078400970526     C                   MOVEL     FUV(10)       FUN004
078500970526     C                   MOVEL     FUV(11)       FUN005
078600970526     C                   MOVEL     FUV(12)       FUN006
078700970526     C                   END
078800970526     C*
078900970526     C* TERZO GRUPPO DI 6 FUNZIONI
079000970526     C     FUNGRU        IFEQ      3
079100970526     C                   MOVEL     FUV(13)       FUN001
079200970526     C                   MOVEL     FUV(14)       FUN002
079300970526     C                   MOVEL     FUV(15)       FUN003
079400970526     C                   MOVEL     FUV(16)       FUN004
079500970526     C                   MOVEL     FUV(17)       FUN005
079600970526     C                   MOVEL     FUV(18)       FUN006
079700970526     C                   END
079800970526     C*
079900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
080000970526     C     FUNGRU        IFEQ      4
080100970526     C                   MOVEL     FUV(19)       FUN001
080200970526     C                   MOVEL     FUV(20)       FUN002
080300970526     C                   MOVEL     FUV(21)       FUN003
080400970526     C                   MOVEL     FUV(22)       FUN004
080500970526     C                   MOVEL     *BLANKS       FUN005
080600970526     C                   MOVEL     *BLANKS       FUN006
080700970526     C                   END
080800970526     C*
080900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
081000970526     C                   MOVEL     *BLANKS       FUN007
081100970526     C                   MOVEL     *BLANKS       FUN008
081200970526     C*
081300970526     C     1             DO        24            I
081400970526     C                   MOVEL     FUD(I)        FUDRIG
081500970526     C     FUDFUN        IFEQ      '24'
081600970526     C     FUDATT        ANDEQ     'S'
081700970526     C                   MOVEL     FUDDES        FUN008
081800970526     C                   END
081900970526     C                   END                                                    DO
082000970526     C*
082100970526     C                   ENDSR
082200970526     C*------------------------------------------------------------------------*
082300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
082400970526     C*------------------------------------------------------------------------*
082500970526     C     FUNCON        BEGSR
082600970526     C*
082700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
082800970526     C                   EXSR      FUNVAR
082900970526     C*
083000970526     C* PULISCO CAMPI DI LAVORO
083100970526     C                   Z-ADD     0             I                 5 0
083200970526     C                   Z-ADD     0             W                 5 0
083300970526     C*
083400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
083500970526     C     FUNKEY        IFNE      *BLANKS
083600970526     C     1             DO        24            I
083700970526     C                   MOVEL     FUD(I)        FUDRIG
083800970526     C*
083900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
084000970526     C     FUNKEY        IFEQ      FUDFUN
084100970526     C     FUDATT        ANDEQ     'S'
084200970526     C                   Z-ADD     I             W
084300970526     C                   END
084400970526     C                   END                                                    DO WHILE
084500970526     C*
084600970526     C* FUNZIONE NON ATTIVA (ERRORE)
084700970526     C     W             IFEQ      0
084800970526     C*
084900970526     C* EMISSIONE MESSAGGIO DI ERRORE
085000970526     C                   MOVEL     MSG(1)        DSMSMS
085100970526     C*
085200970526     C                   END                                                    W=0
085300970526     C                   END                                                    FUNKEY NE *BLANKS
085400970526     C*
085500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
085600970526     C     W             IFNE      0
085700970526     C     FUNKEY        OREQ      *BLANKS
085800970526     C     FUNKEY        IFEQ      *BLANKS
085900970526     C                   EXSR      ENTGES
086000970526     C                   END
086100970526     C     FUNKEY        IFEQ      '03'
086200970526     C                   EXSR      F03GES
086300970526     C                   END
086400970526     C     FUNKEY        IFEQ      '06'
086500970526     C                   EXSR      F06GES
086600970526     C                   END
086700970526     C     FUNKEY        IFEQ      '12'
086800970526     C                   EXSR      F12GES
086900970526     C                   END
087000970526     C     FUNKEY        IFEQ      '24'
087100970526     C                   EXSR      F24GES
087200970526     C                   END
087300970526     C                   END
087400970526     C*
087500970526     C                   ENDSR
087600970526     C*------------------------------------------------------------------------*
087700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
087800970526     C*------------------------------------------------------------------------*
087900970526     C     FUNVAR        BEGSR
088000970526     C*
088100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
088200970526     C*
088300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
088400970526     C     KEY           IFEQ      ENTER
088500970526     C                   MOVEL     *BLANKS       FUNKEY
088600970526     C                   END
088700970526     C     KEY           IFEQ      F1
088800970526     C                   MOVEL     '01'          FUNKEY
088900970526     C                   END
089000970526     C     KEY           IFEQ      F2
089100970526     C                   MOVEL     '02'          FUNKEY
089200970526     C                   END
089300970526     C     KEY           IFEQ      F3
089400970526     C                   MOVEL     '03'          FUNKEY
089500970526     C                   END
089600970526     C     KEY           IFEQ      F4
089700970526     C                   MOVEL     '04'          FUNKEY
089800970526     C                   END
089900970526     C     KEY           IFEQ      F5
090000970526     C                   MOVEL     '05'          FUNKEY
090100970526     C                   END
090200970526     C     KEY           IFEQ      F6
090300970526     C                   MOVEL     '06'          FUNKEY
090400970526     C                   END
090500970526     C     KEY           IFEQ      F7
090600970526     C                   MOVEL     '07'          FUNKEY
090700970526     C                   END
090800970526     C     KEY           IFEQ      F8
090900970526     C                   MOVEL     '08'          FUNKEY
091000970526     C                   END
091100970526     C     KEY           IFEQ      F9
091200970526     C                   MOVEL     '09'          FUNKEY
091300970526     C                   END
091400970526     C     KEY           IFEQ      F10
091500970526     C                   MOVEL     '10'          FUNKEY
091600970526     C                   END
091700970526     C     KEY           IFEQ      F11
091800970526     C                   MOVEL     '11'          FUNKEY
091900970526     C                   END
092000970526     C     KEY           IFEQ      F12
092100970526     C                   MOVEL     '12'          FUNKEY
092200970526     C                   END
092300970526     C     KEY           IFEQ      F13
092400970526     C                   MOVEL     '13'          FUNKEY
092500970526     C                   END
092600970526     C     KEY           IFEQ      F14
092700970526     C                   MOVEL     '14'          FUNKEY
092800970526     C                   END
092900970526     C     KEY           IFEQ      F15
093000970526     C                   MOVEL     '15'          FUNKEY
093100970526     C                   END
093200970526     C     KEY           IFEQ      F16
093300970526     C                   MOVEL     '16'          FUNKEY
093400970526     C                   END
093500970526     C     KEY           IFEQ      F17
093600970526     C                   MOVEL     '17'          FUNKEY
093700970526     C                   END
093800970526     C     KEY           IFEQ      F18
093900970526     C                   MOVEL     '18'          FUNKEY
094000970526     C                   END
094100970526     C     KEY           IFEQ      F19
094200970526     C                   MOVEL     '19'          FUNKEY
094300970526     C                   END
094400970526     C     KEY           IFEQ      F20
094500970526     C                   MOVEL     '20'          FUNKEY
094600970526     C                   END
094700970526     C     KEY           IFEQ      F21
094800970526     C                   MOVEL     '21'          FUNKEY
094900970526     C                   END
095000970526     C     KEY           IFEQ      F22
095100970526     C                   MOVEL     '22'          FUNKEY
095200970526     C                   END
095300970526     C     KEY           IFEQ      F23
095400970526     C                   MOVEL     '23'          FUNKEY
095500970526     C                   END
095600970526     C     KEY           IFEQ      F24
095700970526     C                   MOVEL     '24'          FUNKEY
095800970526     C                   END
095900970526     C     KEY           IFEQ      ROLLUP
096000970526     C                   MOVEL     'RU'          FUNKEY
096100970526     C                   END
096200970526     C     KEY           IFEQ      HELP
096300970526     C                   MOVEL     'HP'          FUNKEY
096400970526     C                   END
096500970526     C                   ENDSR
096600970526     C*------------------------------------------------------------------------*
096700970526     C* NOTE COSTRUZIONE SCHIERE
096800970526     C*------------------------------------------------------------------------*
096900970526     C*
097000970526     C* -MSG-MESSAGGI
097100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
097200970526     C* (NON TOCCARE MSG,1 E MSG,2)
097300970526     C*
097400970526     C* -CMD-COMANDI
097500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
097600970526     C*
097700970526     C* -OPD-OPZIONI
097800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
097900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
098000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
098100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
098200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
098300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
098400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
098500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
098600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
098700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
098800970526     C*
098900970526     C* -FUD-FUNZIONI
099000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
099100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
099200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
099300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
099400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
099500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
099600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
099700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
099800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
099900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
100000970526     O*------------------------------------------------------------------------*
100100970526** MSG - MESSAGGI
100200040628TIS7W4R- Tasto funzionale non gestito                                          1
100300040628TIS7W4R-                                                                       2
100400040628TIS7W4R-                                                                       3
100500040628TIS7W4R-                                                                       4
100600040628TIS7W4R-                                                                       5
100700040628TIS7W4R-                                                                       6
100800040628TIS7W4R-                                                                       7
100900040628TIS7W4R-                                                                       8
101000040628TIS7W4R-                                                                       9
101100040628TIS7W4R-                                                                       10
101200040628TIS7W4R-                                                                       11
101300040628TIS7W4R-                                                                       12
101400040628TIS7W4R-                                                                       13
101500040628TIS7W4R- Data errata                                                           14
101600040628TIS7W4R- La data inizio deve essere maggiore della data fine                   15
101700040628TIS7W4R- Cliente inserito inesistente.                                         16
101800040628TIS7W4R- Coda di stampa inesistente.                                           17
101900050518TIS7W4R- Tipo legame inesistente o mancante.                                   18
102000050518TIS7W4R- Cliente mancante.                                                     19
102100050519TIS7W4R- Immettere una data più recente. Data minima ammessa:xx/xx/xxxx        20
102200040628TIS7W4R- Scelta obbligatoria.                                                  21
102300091111TIS7W4R- Indicare almeno un tipo servizio (C/D/E/H).                           22
102400040628TIS7W4R-                                                                       23
102500040628TIS7W4R-                                                                       24
102600970526** FUD - FUNZIONI
102700970526                   -        (S/N)=N                        funz.01¬
102800970526                   -        (S/N)=N                        funz.02¬
102900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
103000970526                   -        (S/N)=N                        funz.04¬
103100970526                   -        (S/N)=N                        funz.05¬
103200991214F06=Conferma       -        (S/N)=S                        funz.06¬
103300970527                   -        (S/N)=N                        funz.07¬
103400970527                   -        (S/N)=N                        funz.08¬
103500970527                   -        (S/N)=N                        funz.09¬
103600970527                   -        (S/N)=N                        funz.10¬
103700970526                   -        (S/N)=N                        funz.11¬
103800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
103900970526                   -        (S/N)=N                        funz.13¬
104000970526                   -        (S/N)=N                        funz.14¬
104100970526                   -        (S/N)=N                        funz.15¬
104200970526                   -        (S/N)=N                        funz.16¬
104300970526                   -        (S/N)=N                        funz.17¬
104400970526                   -        (S/N)=N                        funz.18¬
104500970526                   -        (S/N)=N                        funz.19¬
104600970526                   -        (S/N)=N                        funz.20¬
104700970526                   -        (S/N)=N                        funz.21¬
104800970526                   -        (S/N)=N                        funz.22¬
104900970526                   -        (S/N)=N                        funz.23¬
105000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
