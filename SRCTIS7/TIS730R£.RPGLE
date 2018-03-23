000100970526     H*------------------------------------------------------------------------*
000200000714     H* Iscrizione clienti a strategi/FTP
000300970526     H*------------------------------------------------------------------------*
000400100930     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900000714     FtiS730d   cf   e             WORKSTN INFDS(DEVDS1)
001000000714     F                                     SFILE(S730DMS:MSGULT)
001100970527     F*------------
001200991104     Ftivss01L  UF A E           K DISK
001300001116     Ftivss02L  IF   E           K DISK    RENAME(tivss000:tivss002)
001400100930     Ftiabl01l  UF A E           K DISK
001500020228     Ftikun06L  IF   E           K DISK
001600091027     Ftntbe01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000000717     D* CREAZOINE PASSWORD CLINTE
002100000717     D*------------
002200000717     D PASS            S              1    DIM(30)
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600000714     D DEC             S             66    DIM(1) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000100930     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI TASTI FUNZIONALI
003300970526     D*------------
003400970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* FUNZIONI DA CARICARE A VIDEO
003700970526     D*------------
003800970526     D FUV             S             19    DIM(24)
003900970526     D*------------------------------------------------------------------------*
004000970526     D* INPUT
004100970526     D*------------------------------------------------------------------------*
004200970526     D*------------
004300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004400970526     D*------------
004500970526     D FUDDS           DS
004600970526     D  FUDDE1                 1      1
004700970526     D  FUDFUN                 2      3
004800970526     D  FUDDE2                 4     19
004900970526     D  FUDFI0                20     34
005000970526     D  FUDATT                35     35
005100970526     D  FUDFI1                36     80
005200970526     D  FUDDES                 1     19
005300970526     D  FUDRIG                 1     80
005400970526     D*------------
005500020228     D* DS GESTIONE TASTI FUNZIONALI
005600970526     D*------------
005700970526     D DSKEY         E DS
005800970526     D DEVDS1          DS
005900970526     D  KEY                  369    369
006000970526     D*------------
006100970526     D* DS "XMSG" - GESTIONE MESSAGGI
006200970526     D*------------
006300970526     D DSMSG         E DS
006400970528     D*------------------
006500970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600970528     D*------------------
006700000317     D WLBDA8          DS                  INZ
006800970528     D  G08DAT                 1      8  0
006900970528     D  G08INV                 9     16  0
007000970528     D  G08ERR                17     17
007100970528     D  G08TGI                18     22  0
007200010115     D*------------------
007300010115     D* DS X CONTROLLO INDIRIZZO E-MAIL
007400010115     D*------------
007500010115     D DSEMAIL       E DS
007600970526     D*------------
007700970526     D* DS DI PROCEDURA
007800970526     D*------------
007900010115     D TIS720        E DS                  EXTNAME(TIS720DS)
008000001116     D TIBS02DS      E DS
008100091027     D dtle          E DS
008200970526     D*------------
008300970526     D* ARCHITETTURA
008400970526     D*------------
008500970526     D KPJBA         E DS
008600000714     D*------------
008700000714     D* TABELLE "XCLIR"- RICERCA CLIENTI
008800000714     D ksc             s              7  0 dim(30)
008900000714     D ksa             s              4    dim(30)
009000000714     D*------------------
009100000714     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
009200000714     D*------------------
009300000714     DDSXC           e DS                  extname(XCLIRDS)
009400000714     D*---
009500000714     D                 DS
009600000714     D dsksn                   1      4p 0
009700000714     D dsksa                   1      4
009800000714
009900000714     DBS69DS         e ds                  extname(TIBS69DS)
010000000714     DACODS          e ds                  extname(cnaco00f)
010100000714     DINDDS          e ds                  extname(cnind00f)
010200000714     DCLPDS          e ds                  extname(cnclp00f)
010300000714     DCLSDS          e ds                  extname(fncls00f)
010400000714     D*------------------
010500000714     D* VARIABILI DI WRK
010600000714     D*------------------
010700000714     D DATAI           s              8    inz(*all'0')
010800000714     D DATAF           s              8    inz(*all'0')
010900000717     D MESS            s             80    inz(*blanks)
011000000719     D API             s              1    inz('''')
011100000719     D POS             s              2  0 inz(*zeros)
011200000719     D ALFABETO        s             26    inz('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
011300001116     D vssok           s               N
011400091027     D sktle           S              2    DIM(20)
011500091027     D jtle            S              5  0                                      *indici
011600020924     D*-------------------
011700020924     D* DS REPERIMENTO DATI UTENTE
011800020924     D*-------------------
011900020924     D TIBS34DS      E DS                                                       *Profili utente
012000020924     D DDATIUTE      E DS                                                       *Dati utente
012100020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012200050105
012300120213     d Tis730ds      e ds                  prefix(in_)
012400120213     d Tis730ds_w    e ds                  extname(tis730ds)
012500050105
012600050105     D                SDS
012700050105     D  PARMS                 37     39  0
012800100930
012900100930     D/COPY GAITRASRC/SRCPROTOPR,UBRANDOM
013000100930     D/COPY GAITRASRC/SRCPROTOPI,UBRANDOM
013100100930
013200000714
013300970526     C*------------------------------------------------------------------------*
013400970526     C* MAIN LINES
013500970526     C*------------------------------------------------------------------------*
013600100930      *
013700120213     c                   clear                   tis730ds_w
013800100930     c                   setoff                                       71
013900110112     c                   Movel     *blanks       ablsun
014000100930      *
014100050105      * se pgm richiamato vado subito alla routine richiamata dal tasto funzione F6
014200050105     c                   If        wricpgm <> *Blanks
014300120213     c                   eval      tis730ds_w = tis730ds
014400100930     c                   select
014500100930     c* - inserimento
014600100930     c                   when      S30OPZ = 'II'
014700050105     c                   ExSr      movs30
014800050105     c                   ExSr      inseri
014900101018     c* - modifica
015000101018     c                   when      S30OPZ = 'UU'
015100100930     c                   seton                                        71
015200100930     c                   ExSr      movs30
015300101018     c                   ExSr      modifi
015400100930     c                   endsl
015500100930     c*
015600120213     c                   eval      tis730ds = tis730ds_w
015700050111     c                   Goto      fine
015800050105     c                   EndIf
015900970526     C*
016000970526     C* NON CI SONO ERRORI IN ENTRATA
016100001116     C     ERRL00        IFEQ      '0'
016200970526     C*
016300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016400001116     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016700970526     C*
016800970526     C* EMETTE FORMATO VIDEO
016900970526     C                   EXSR      BUTFMR
017000970526     C*
017100970526     C* GESTIONE TASTI FUNZIONALI
017200970526     C                   EXSR      FUNCON
017300001116     C                   ENDDO
017400970526     C*
017500970526     C* CI SONO ERRORI IN ENTRATA
017600001116     C                   ELSE
017700991104     C                   MOVEL     '1'           s20err
017800991104     C                   MOVEL     DSMSMS        s20msg
017900001116     C                   ENDIF
018000970526     C* FINE PGM
018100050111     c     fine          tag
018200001116     C                   EXSR      FINSR
018300001116     C*
018400970526     C                   SETON                                        LR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     BUTFMR        BEGSR
018900970526     C*
019000970526     C* EMETTE LA TESTATA
019100000714     C                   WRITE     S730D01
019200970526     C*
019300970526     C* SPEDISCE EVENTUALE MESSAGGIO
019400970526     C                   EXSR      SNDMSG
019500970526     C*
019600970526     C* EMETTE LE FUNZIONI
019700000714     C                   WRITE     S730D02
019800970526     C*
019900970526     C* EMETTE IL VIDEO
020000000714     C                   EXFMT     S730D03
020100970526     C*
020200970526     C* REIMPOSTA FLAG DI ERRORE
020300970526     C                   MOVEL     '0'           ERRL00
020400970526     C*
020500970526     C* REIMPOSTA INDICATORI DI ERRORE
020600000714     C                   SETOFF                                       616263
020700010227     C                   SETOFF                                       6465
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
021200970526     C*------------------------------------------------------------------------*
021300970526     C     SNDMSG        BEGSR
021400970526     C*
021500970526     C* PULISCE IL SFILE MESSAGGI
021600970526     C                   MOVEL     '1'           *IN40
021700000714     C                   WRITE     S730DMC
021800970526     C                   MOVEL     '0'           *IN40
021900970526     C*
022000970526     C* VISUALIZZA IL MESSAGGIO
022100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
022200970526     C                   CALL      'XMSG'        XMSGPA
022300001116     C     DSMSER        IFEQ      *ZEROS
022400970526     C                   MOVEL     DSMSKE        MSGKEY
022500001116     C                   ENDIF
022600970526     C                   Z-ADD     1             MSGULT
022700000714     C                   WRITE     S730DMS
022800000714     C                   WRITE     S730DMC
022900970526     C*
023000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
023100970526     C                   MOVEL     *BLANKS       DSMSMS
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* ENTGES - GESTIONE ENTER -> INVIO
023600970526     C*------------------------------------------------------------------------*
023700970526     C     ENTGES        BEGSR
023800970526     C*
023900970526     C* CONTROLLA I CAMPI IMMESSI
024000970526     C                   EXSR      DECODI
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* F03GES - GESTIONE F03 -> FINE
024500970526     C*------------------------------------------------------------------------*
024600970526     C     F03GES        BEGSR
024700970526     C*
024800970526     C* IMPOSTA "PREMUTO F03"
024900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
025000991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
025100970526     C*
025200970526     C                   ENDSR
025300001116     C*------------------------------------------------------------------------*
025400001116     C* F05GES - GESTIONE F05 -> REFRESH
025500001116     C*------------------------------------------------------------------------*
025600001116     C     F05GES        BEGSR
025700001116     C*
025800001116     C* LANCIA LA ROUTINE DI AZZERAMENTO VIEO
025900001116     C                   EXSR      AZZVID
026000001116     C*
026100001116     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300010605     C* F06GES - GESTIONE F06 -> INSERIMENTO
026400970526     C*------------------------------------------------------------------------*
026500970526     C     F06GES        BEGSR
026600970526     C*
026700970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
026800970526     C                   EXSR      INSERI
026900010605     C*
027000010605     C* LANCIA LA ROUTINE DI AZZERAMENTO VIDEO
027100010605     C                   EXSR      AZZVID
027200001116     C*
027300970526     C                   ENDSR
027400001116     C*------------------------------------------------------------------------*
027500001116     C* F11GES - GESTIONE F11 -> PASSAGGIO A GESTIONE ABILITAZIONE AI SERVIZI
027600001116     C*------------------------------------------------------------------------*
027700001116     C     F11GES        BEGSR
027800001116     C*
027900001116     C* PASSA ALLA GESTIONE ABILITAZIONI
028000001116     C                   CLEAR                   tis720
028100001116     C                   MOVEL     'P02'         s20OP0                         *LIVELLO PROCEDURA
028200001116     C                   MOVEL     'O09'         s20OP1                         *LIVELLO PGM
028300001116     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
028400001116     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
028500001116     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
028600001116     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
028700001116     C                   MOVEL     'WW'          s20tle                         *TIPO LEGAME
028800001116     C                   MOVE(p)   VIDksu        s20ksc                         *CLIENTE UNIFICANTE
028900001116     C*
029000001116     C                   MOVEL     tis720        KPJBU
029100001116     C                   CALL      'TIS724R'
029200001116     C                   PARM                    KPJBA
029300001116     C*
029400001116     C* LANCIA LA ROUTINE DI AZZERAMENTO VIEO
029500001116     C                   EXSR      AZZVID
029600001116     C*
029700001116     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900970526     C* F12GES - GESTIONE F12 -> RITORNO
030000970526     C*------------------------------------------------------------------------*
030100970526     C     F12GES        BEGSR
030200970526     C*
030300970526     C* IMPOSTA "PREMUTO F12"
030400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
030500991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
030600970526     C*
030700970526     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
031000970526     C*------------------------------------------------------------------------*
031100970526     C     F24GES        BEGSR
031200970526     C*
031300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
031800970526     C*------------------------------------------------------------------------*
031900970526     C     INSERI        BEGSR
032000970526     C*
032100970526     C* CONTROLLA I CAMPI IMMESSI
032200050105      * se non è richiamato
032300050105     c                   if        wricpgm = *Blanks
032400970526     C                   EXSR      DECODI
032500050105     c                   EndIf
032600970526     C*
032700000714     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032800000719     C* IMPOSTA I CAMPI DEL RECORD "VSS" DA SCRIVERE
032900000719     C                   CLEAR                   tivss000
033000000714     C                   MOVEL     VIDksu        vssksu
033100001116     C                   MOVEL     VIDisv        vssisv
033200020307     C                   MOVE(p)   VIDDAI        vssdde
033300020307     C                   MOVE(p)   VIDDAF        vssdsc
033400000714     C                   Z-ADD     *ZEROS        vssdtr                         *DATA TRASMISSIONE
033500000714     C                   MOVEL     *BLANKS       vssftr                         *FLAG TRASMISSIONE
033600100930     C* IMPOSTA I CAMPI DEL RECORD "ABL" DA SCRIVERE
033700000719     C                   CLEAR                   tiabl000
033800100930     C                   EXSR      crtpwd
033900100930     C                   EVAL      operazione = 'I'
034000100930     C                   EXSR      EXESGI
034100100930     C*
034200000714     C* SOLO SE TUTTO OK
034300010605     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034400001116     C                   WRITE     tivss000
034500001116     C                   WRITE     tiabl000
034600001116     C                   ENDIF
034700050105     C                   ENDIF
034800970526     C*
034900970526     C                   ENDSR
035000970526     C*------------------------------------------------------------------------*
035100001116     C* DECODI - CONTROLLA CAMPI E DECODIFICA
035200970526     C*------------------------------------------------------------------------*
035300970526     C     DECODI        BEGSR
035400000714     C*
035500000714     C                   SETOFF                                       616263
035600010227     C                   SETOFF                                       6465
035700001116     C*
035800970528     C*--------------------
035900000714     C* CLIENTE
036000970528     C*--------------------
036100991105     C     *LIKE         DEFINE    vssksu        ksuVID
036200001116     C     ERRL00        IFEQ      '0'
036300991105     C                   MOVEL     *BLANKS       VDDksu
036400001116     C     VIDksu        IFEQ      *blanks                                      *STANDARD
036500001116     C     VIDksu        OREQ      *zeros                                       *STANDARD
036600000705     C                   MOVEL     '1'           ERRL00
036700000705     C                   SETON                                        61
036800000714     C                   MOVEL     MSG(4)        DSMSMS
036900991105     C                   MOVEL     *ZEROS        ksuVID
037000001116     C                   ELSE
037100041230     C     VIDksu        IFGT      '09999999'                                   *STANDARD
037200041230     C                   MOVEL     '1'           ERRL00
037300041230     C                   SETON                                        61
037400041230     C                   MOVEL     MSG(1)        DSMSMS
037500041230     C                   MOVEL     *ZEROS        ksuVID
037600041230     C                   ELSE
037700991105     C                   MOVEL     VIDksu        Wksu
037800991105     C                   MOVEL     *blank        Wdksu
037900991105     C                   EXSR      CHKksu
038000991105     C                   MOVEL     Wksu          VIDksu
038100991105     C                   MOVEL     WDksu         VDDksu
038200991105     C                   MOVEL     VIDksu        ksuVID
038300001116     C     ERRL00        IFEQ      '1'
038400970528     C                   SETON                                        61
038500000714     C                   MOVEL     MSG(1)        DSMSMS
038600020228     C                   ELSE
038700091027     C* Che nn sia cliente figlio
038800020228     C                   MOVE(p)   VIDksu        KUNcof
038900091027     c*
039000091027     c* ciclo x tutti i legami d tipo "internet"
039100091027     c                   z-add     1             jtle
039200091027     c                   dow       jtle <= %elem(sktle)
039300091027     c                   if        sktle(jtle) <> *blanks
039400091027     c*
039500091027     C                   movel     sktle(jtle)   kuntle
039600020228     C     KE6kun        SETLL     tikun06l
039700020228     C     KE6kun        READE     tikun06l
039800020228     C                   DOW       not %eof(tikun06l)
039900020228     C     KUNvat        IFEQ      *blanks                                      *annullati/sospesi
040000020307     C                   IF        KUNdsc <  VIDDAI  OR                         *non valido
040100020307     C                             KUNdde >  VIDDAF
040200020228     C                   ELSE                                                   *valido
040300020409     C                   MOVEL     '1'           ERRL00
040400020228     C                   SETON                                        61
040500020228     C                   MOVEL     MSG(13)       DSMSMS
040600020228     C                   LEAVE
040700020228     C                   ENDIF
040800020228     C                   ENDIF
040900020228     C     KE6kun        READE     tikun06l
041000020228     C                   ENDDO
041100091027     c*
041200091027     c                   else
041300091027     c                   leave
041400091027     c                   endif
041500091027     c*
041600091027     c                   add       1             jtle
041700091027     c                   enddo
041800091027     c*
041900001116     C                   ENDIF
042000001116     C                   ENDIF
042100001116     C                   ENDIF
042200041230     C                   ENDIF
042300000714     C*
042400000714     C*--------------------
042500000714     C* RESPONSABILE
042600000714     C*--------------------
042700001116     C     ERRL00        IFEQ      '0'
042800000714     C     VIDrsp        IFEQ      *blanks
042900000714     C                   MOVEL     '1'           ERRL00
043000000714     C                   SETON                                        62
043100000714     C                   MOVEL     MSG(4)        DSMSMS
043200000714     C                   ENDIF
043300000714     C                   ENDIF
043400000714     C*
043500000714     C*--------------------
043600000714     C* E-MAIL RESPONSABILE
043700000714     C*--------------------
043800001116     C     ERRL00        IFEQ      '0'
043900000714     C     VIDem1        IFEQ      *blanks
044000000714     C     VIDem2        ANDEQ     *blanks
044100000714     C                   MOVEL     '1'           ERRL00
044200000714     C                   SETON                                        63
044300001116     C                   MOVEL     MSG(4)        DSMSMS
044400010115     C                   ELSE
044500010115     C* Lancio utility di controllo formale indirizzo e-mail
044600010115     C                   CLEAR                   DSEMAIL
044700010115     C                   EVAL      §EMLINDI = VIDem1+VIDem2
044800010115     C                   CALL      'XEMAIL'
044900010115     C                   PARM                    DSEMAIL
045000010115     C                   IF        §EMLERRO = '0'
045100010115     C                   EVAL      VIDem1 = %subst(§EMLINDO:1:49)
045200010115     C                   EVAL      VIDem2 = %subst(§EMLINDO:50:49)
045300010115     C                   ELSE
045400010115     C                   SETON                                        63
045500010115     C                   MOVEL     §EMLMSGO      DSMSMS
045600010115     C                   MOVEL     '1'           ERRL00
045700010115     C                   ENDIF
045800010115     C                   ENDIF
045900010115     C                   ENDIF
046000001116     C*
046100001116     C*--------------------
046200001116     C* TIPO SERVIZIO
046300001116     C*--------------------
046400001116     C     ERRL00        IFEQ      '0'
046500001116     C     VIDisv        IFNE      *blank
046600001116     C                   EXSR      CNTISV
046700001116     C                   ELSE
046800041230     C                   MOVEL     '1'           ERRL00
046900041230     C                   SETON                                        64
047000041230     C                   MOVEL     MSG(4)        DSMSMS
047100001116     C                   MOVEL     *blanks       VDDisv
047200001116     C                   ENDIF
047300001116     C                   ENDIF
047400001116     C*
047500001116     C*--------------------
047600001116     C* CONTROLLO VALIDA AI FINI DELL'INSERIMENTO DELL'ABILITAZIONE AL SERVIZIO RICHIESTO
047700001116     C*--------------------
047800001116     C     ERRL00        IFEQ      '0'
047900001116     C                   EVAL      vssok = *off
048000001116     C                   EVAL      KCEksu = VIDksu
048100001116     C                   EVAL      KCEisv = VIDisv
048200001116     C     KE2vss        SETLL     tivss02l
048300001116     C     KE2vss        READE     tivss02l
048400001116     C                   DOW       not %eof
048500001116     C                   IF        vssdde <= DATCOR and
048600001116     C                             vssdsc >= DATCOR and
048700001116     C                             vssvat <> 'A'    and
048800001116     C                             vssvat <> 'S'
048900001116     C                   EVAL      vssok = *on
049000001116     C                   LEAVE
049100001116     C                   ENDIF
049200001116     C     KE2vss        READE     tivss02l
049300001116     C                   ENDDO
049400001116     C*
049500001116     C                   IF        vssok = *on
049600001116     C                   MOVEL     '1'           ERRL00
049700001116     C                   SETON                                        6164
049800001220     C                   EVAL      DSMSMS = %TRIM(MSG(3)) + ' ' + ablsun
049900001116     C                   ENDIF
050000001116     C                   ENDIF
050100001116     C*
050200000714     C                   ENDSR
050300970528     C*------------------------------------------------------------------------*
050400991105     C* CHKksu - CONTROLLA codice padre figlio
050500970528     C*------------------------------------------------------------------------*
050600991105     C     CHKksu        BEGSR
050700970528     C*
050800991105     C     *LIKE         DEFINE    VIDksu        Wksu
050900991105     C     *LIKE         DEFINE    VDDksu        WDksu
051000000714     C                   MOVE      *zeros        wksc
051100970528     C*
051200970528     C* RICHIESTO ELENCO
051300970528     C                   SETOFF                                           28
051400991105     C     '?'           SCAN      Wksu                                   28
051500001116     C     *IN28         IFEQ      *ON
051600000714     C* RICHIESTO ELENCO CODICI CLIENTI
051700991008     C                   CLEAR                   DSXC
051800020924     C                   Z-ADD     DUTKCI        DXCCAP
051900991008     C                   CALL      'XCLIR'
052000991008     C                   PARM                    DSXC
052100991008     C* SCOMPATTA
052200991008     C                   MOVEA     DXCKSC        KSA                        ALF
052300001116     C     1             DO        30            J                 3 0
052400991008     C                   MOVE      KSA(J)        DSKSA                      F
052500991008     C                   Z-ADD     DSKSN         KSC(J)                     M
052600001116     C                   ENDDO
052700991011     C                   MOVEL     KSC(1)        WKSC              7
052800991011     C                   MOVEL     '1'           $TESVD            1        TA
052900001116     C                   ENDIF
053000991008     C*
053100991008     C* CODICE CLIENTE NUMERICO
053200001116     C     WKSC          IFLT      *ZEROS
053300991011     C                   MOVEL     '1'           WERR              1
053400001116     C                   ELSE
053500970528     C*
053600970528     C* SOLO CARATTERI NUMERICI
053700000714     C   28              MOVE      *all'0'       wksu
053800000714     C   28              MOVE      wksc          wksu
053900970528     C                   SETOFF                                       28
054000991105     C                   TESTN                   Wksu                 28
054100001116     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
054200970528     C                   MOVEL     '1'           ERRL00
054300001116     C                   ELSE                                                   *E'NUMERO DA TESTN
054400991105     C                   MOVE      Wksu          W001              1
054500001116     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
054600970528     C                   MOVEL     '1'           ERRL00
054700001116     C                   ELSE                                                   *NUMERO
054800970528     C*
054900970528     C* CONTROLLA ESISTENZA CODICE
055000991210     C                   MOVE      Wksu          wcli              7 0
055100000714     C                   EXSR      DECTIB
055200001116     C                   ENDIF
055300001116     C                   ENDIF
055400001116     C                   ENDIF
055500970528     C*
055600970528     C                   ENDSR
055700001116     C*------------------------------------------------------------------------*
055800001116     C* CNTISV - CONTROLLA tipo servizio a cui il cliente deve essere abilitato
055900001116     C*------------------------------------------------------------------------*
056000001116     C     cntisv        begsr
056100001116     C                   CLEAR                   TIBS02DS
056200001116     C*
056300001116     C     '?'           SCAN      VIDisv                                 30
056400001116     C*  Ricerca
056500001116     C                   EVAL      T02TLA = 'L'
056600001116     C                   IF        *IN30 = *ON
056700001116     C                   MOVEL     'R'           T02MOD
056800001116     C                   ELSE
056900001116     C                   MOVEL     'C'           T02MOD
057000001116     C                   MOVEL(P)  VIDisv        T02KE1
057100001116     C                   ENDIF
057200001116     C                   MOVEL     'ISV'         T02COD
057300001116     C                   MOVEL     KNSIF         T02SIF
057400001116     C                   CALL      'TIBS02R'
057500001116     C                   PARM                    KPJBA
057600001116     C                   PARM                    TIBS02DS
057700001116     C
057800001116     C*
057900001116     C                   IF        T02ERR = *blanks
058000001116     C                   MOVEL     T02KE1        VIDisv
058100001116     C                   MOVEL     T02UNi        VDDisv
058200010227     C                   IF        VIDisv = 'TD'
058300010227     C                   SETON                                        65
058400010227     C                   ENDIF
058500001116     C                   ELSE
058600001116     C                   MOVEL     '1'           ERRL00
058700001116     C                   SETON                                        64
058800001116     C                   MOVEL     MSG(10)       DSMSMS
058900001116     C                   ENDIF
059000001116     C
059100001116     C                   endsr
059200991210     C*------------------------------------------------------------------------*
059300000714     C* DECTIB - Reperimento RAgione Sociale del Cliente
059400991210     C*------------------------------------------------------------------------*
059500000714     C     DECTIB        BEGSR
059600000714     C*
059700991210     C                   CLEAR                   BS69DS
059800991210     C                   CLEAR                   ACODS
059900991210     C                   MOVEL     KNSIF         I69SIF
060000991210     C                   Z-ADD     WCLI          I69KAC
060100991210     C                   CALL      'TIBS69R'
060200991210     C                   PARM                    BS69DS
060300991210     C                   PARM                    ACODS
060400991210     C                   PARM                    INDDS
060500991210     C                   PARM                    CLPDS
060600991210     C                   PARM                    CLSDS
060700991210     C     O69ERR        IFNE      '1'
060800991210     C                   MOVEL     ACORAG        wdksu
060900991210     C                   ELSE
061000991210     C                   MOVEL     *ALL'*'       Wdksu
061100000714     C                   MOVE      '1'           ERRL00
061200991210     C                   ENDIF
061300000714     C*
061400000714     C                   ENDSR
061500991210     C*------------------------------------------------------------------------*
061600991210     C* FINSR - OPERAZIONI FINALI
061700991210     C*------------------------------------------------------------------------*
061800991210     C     FINSR         BEGSR
061900991210     C*
062000991210     C* CHIUDE I FILE DEI PGM CHIAMATI
062100991210     C                   CLEAR                   bs69ds
062200991210     C                   CLEAR                   acods
062300991210     C                   CLEAR                   indds
062400991210     C                   CLEAR                   clpds
062500991210     C                   CLEAR                   clsds
062600991210     C                   MOVEL     'C'           i69tla
062700991210     C                   MOVEL     knsif         i69sif
062800991210     C                   CALL      'TIBS69R'
062900991210     C                   PARM                    bs69ds
063000991210     C                   PARM                    acods
063100991210     C                   PARM                    indds
063200991210     C                   PARM                    clpds
063300991210     C                   PARM                    clsds
063400991210     C*
063500991210     C                   ENDSR
063600970526     C*------------------------------------------------------------------------*
063700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
063800970526     C*------------------------------------------------------------------------*
063900970526     C     AZZVID        BEGSR
064000970526     C* CAMPI
064100000714     C                   MOVEL     *BLANKS       VIDksu
064200000714     C                   MOVEL     *BLANKS       VIDrsp
064300000714     C                   MOVEL     *BLANKS       VIDem1
064400000714     C                   MOVEL     *BLANKS       VIDem2
064500001116     C                   MOVEL     'TT'          VIDisv
064600010306     C                   MOVEL     *BLANKS       VIDtel
064700010306     C                   MOVEL     *BLANKS       VIDfax
064800010306     C                   MOVEL     *BLANKS       VIDfax
064900970526     C* DECODIFICHE
065000000714     C                   MOVEL     *BLANKS       VDDksu
065100001116     C                   MOVEL     *BLANKS       VDDisv
065200001116     C* VARIE
065300001116     C                   MOVEL     '0'           CHKOK
065400970526     C*
065500970526     C                   ENDSR
065600970526     C*------------------------------------------------------------------------*
065700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
065800970526     C*------------------------------------------------------------------------*
065900970526     C     IMPFUN        BEGSR
066000001116     C*
066100001116     C* SE OK INSERIMENTO ABILITA FUNZIONE DI "GESTIONE ABILITAZIONI"
066200050105     C*    CHKOK         IFNE      '1'
066300050105     C*                  MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
066400050105     C*                  MOVEL     'N'           FUDATT
066500050105     C*                  MOVEL     FUDRIG        FUD(11)
066600050105     C*                  ELSE
066700050105     C*                  MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
066800050105     C*                  MOVEL     'S'           FUDATT
066900050105     C*                  MOVEL     FUDRIG        FUD(11)
067000050105     C*                  ENDIF
067100050111      * disabilito il tasto funzione F11 se pgm richiamato
067200050111     c                   If        wricpgm <> *Blanks
067300050111     C                   MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
067400050111     C                   MOVEL     'N'           FUDATT
067500050111     C                   MOVEL     FUDRIG        FUD(11)
067600050111     c                   EndIf
067700001116     C*
067800001116     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
067900001116     C     DEPOP1        IFEQ      'O05'
068000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
068100970526     C                   MOVEL     'N'           FUDATT
068200970526     C                   MOVEL     FUDRIG        FUD(6)
068300001116     C                   ENDIF
068400970526     C*
068500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
068600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
068700970526     C                   EXSR      FUNGES
068800970526     C*
068900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
069000970526     C                   MOVEL     DEC(1)        VIDFUN
069100970526     C*
069200970526     C                   ENDSR
069300000717     C*------------------------------------------------------------------------*
069400000717     C* CRTPWD - Creazione Password Cliente 1° accesso
069500000717     C*------------------------------------------------------------------------*
069600000717     C     CRTPWD        BEGSR
069700000717     C*
069800000717     C                   MOVEA     VDDKSU        PASS
069900000721     C                   EXSR      CALORA
070000000721     C                   MOVE(p)   ORA           Alfa4             4
070100000717     C                   MOVEL     *blanks       Alfa1             1
070200100930     C                   MOVEL     *blanks       AlfaRnd           1
070300000717     C                   Z-ADD     *zeros        Num1              1 0
070400101007     C                   EVAL      ablpwd = *blanks
070500000717     C     1             DO        30            x                 2 0
070600000717     C                   EVAL      Alfa1  = PASS(x)
070700000719     C                   Z-ADD     *zeros        pos
070800000719     C                   EVAL      pos = %scan(PASS(x):alfabeto)
070900000719     C                   IF        pos > 0 AND Num1 < 3
071000000717     C                   EVAL      ablpwd = %trim(ablpwd) + Alfa1
071100000717     C                   ADD       1             Num1
071200000717     C                   ENDIF
071300000717     C                   ENDDO
071400100930     C                   EVAL      ablpwd = %trim(ablpwd) + Alfa4
071500100930     C*
071600100930     C* Aggiungo x sicurezza 1 byte "random"
071700100930     C                   EVAL      pInValDa   = 1
071800100930     C                   EVAL      pInValA    = 9
071900100930     C                   EVAL      pOutRandom = *zeros
072000100930     C                   CALLP(e)  UBRANDOM_Retrieve(pInValDa :
072100100930     C                                               pInValA  :
072200100930     C                                               pOutRandom)
072300100930     C*
072400100930     C                   IF        not %error
072500100930     C                   EVAL      AlfaRnd = %trim(%char(pOutRandom))
072600100930     C                   EVAL      ablpwd = %trim(ablpwd) + AlfaRnd
072700100930     C                   ENDIF
072800000717     C*
072900000717     C                   ENDSR
073000000719     C*------------------------------------------------------------------------*
073100000719     C* CALORA - Reperimento data ora di sistema in corso di esecuzione
073200000719     C*------------------------------------------------------------------------*
073300000719     C     CALORA        BEGSR
073400000719     C*
073500000719     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
073600000719     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
073700000719     C                   MOVEL     WN14          ORA               6            *ORA
073800000719     C                   Z-ADD     WN8           G08DAT
073900000719     C                   Z-ADD     *ZEROS        G08INV
074000000719     C                   MOVEL     '0'           G08ERR
074100000719     C                   CALL      'XSRDA8'
074200000719     C                   PARM                    WLBDA8
074300000719     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
074400000719     C                   MOVEL     DATCOR        AAAA              4            *ANNO ALFA
074500000719     C*
074600000719     C                   ENDSR
074700020924     C*--------------------------------------------------------------------------------------------*
074800020924     C* REPERISCE I DATI UTENTE
074900020924     C*--------------------------------------------------------------------------------------------*
075000020924     C     REPDATIUTE    BEGSR
075100020924     C*
075200020924     C* INIZIALIZZA VARIABILI DI WRK
075300020924     C                   CLEAR                   TIBS34DS
075400020924     C                   CLEAR                   AZUTEDS
075500020924     C                   CLEAR                   DDATIUTE
075600020924     C*
075700020924     C     *DTAARA       DEFINE    §azute        azuteds
075800020924     C     *DTAARA       DEFINE    §datiute      ddatiute
075900020924     C                   IN(E)     *DTAARA
076000020924if  1C                   IF        %Error
076100020924     c                   EVAL      I34Tla = 'L'
076200020924     C                   CALL      'TIBS34R'
076300020924     C                   PARM                    Tibs34Ds
076400020924     C                   IN        *DTAARA
076500020924e   1C                   ENDIF
076600020924     C*
076700020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
076800020924     C                   MOVEL(P)  RSUT          DSFIRS           20
076900020924     C*
077000020924     C                   ENDSR
077100050105      *------------------------------------------------------------------------*
077200050105      * MOVS30 - Muovo i campi della ds esterna nei campi video
077300050105      *------------------------------------------------------------------------*
077400050105     c     Movs30        BegSr
077500050105
077600050105     c                   Move      s30ksu        vidksu
077700050105     c                   Eval      vidrsp = s30rsp
077800050105     c                   Eval      videm1 = %subst(s30eml:1:49)
077900050105     c                   Eval      videm2 = %subst(s30eml:50:49)
078000050105     c                   Eval      vidisv = s30isv
078100050105     c                   Eval      viddai = s30dti
078200050105     c                   Eval      viddaf = s30dtf
078300050105     c                   Eval      viddep = s30dep
078400110112     c                   Eval      ablsun = s30sun
078500050105     c                   Clear                   vidtel
078600050105     c                   Clear                   vidfax
078700050105     c                   Move      vidksu        wcli
078800050105     c                   ExSr      Dectib
078900050105     c                   Move      wdksu         vddksu
079000091027     c*
079100050105     c                   EndSr
079200100930     C*------------------------------------------------------------------------*
079300101018     C* MODIFI - OPERAZIONI DI MODIFICA
079400100930     C*------------------------------------------------------------------------*
079500101018     C     MODIFI        BEGSR
079600100930     C*
079700100930     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
079800100930     C                   EXSR      LETabl
079900100930     C*
080000100930     C* SE TUTTO OK => PROCEDO
080100100930     C                   IF        S30ERR = *blanks
080200100930     C                   IF        S30PWD = *blanks
080300100930     C                   EXSR      crtpwd
080400100930     C                   ELSE
080500100930     C                   EVAL      ablpwd = S30PWD
080600100930     C                   ENDIF
080700100930     C*
080800100930     C                   EVAL      operazione = 'A'
080900100930     C                   EXSR      EXESGI
081000100930     C*
081100100930     C* SOLO SE TUTTO OK
081200100930     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
081300100930     C                   UPDATE    tiabl000
081400100930     C                   ENDIF
081500100930     C                   ENDIF
081600100930     C*
081700100930     C                   ENDSR
081800100930     C*------------------------------------------------------------------------*
081900100930     C* EXESGI - OPERAZIONI "LATO SGI"
082000100930     C*------------------------------------------------------------------------*
082100100930     C     EXESGI        BEGSR
082200100930     C*
082300100930     C                   MOVE      VIDksu        ablute
082400100930     C                   EVAL      ablisv = VIDisv
082500100930     C                   EVAL      ableml = %trim(VIDem1) + %trim(VIDem2)
082600100930     C                   EVAL      ablrsp = VIDrsp
082700100930     C                   EVAL      abltip = 'SG1'
082800100930     C                   EVAL      ablinv = '0'
082900100930     C                   EVAL      abltel = VIDtel
083000100930     C                   EVAL      ablfax = VIDfax
083100100930     C                   EVAL      abldep = VIDdep
083200100930     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
083300100930     C                   EVAL      Alfa30 = ablrsp
083400100930     C     api:' '       XLATE     Alfa30        Alfa30
083500100930     C*
083600100930     C                   EVAL      Alfa20 = VDDksu
083700100930     C     api:' '       XLATE     Alfa20        Alfa20
083800100930     C*
083900100930     C                   CALL(e)   'TIS730C1'
084000100930     C                   PARM                    ablute
084100100930     C                   PARM                    ablpwd
084200100930     C                   PARM                    alfa20           20
084300100930     C                   PARM                    alfa30           30
084400110112     C                   PARM                    ablsun
084500100930     C                   PARM      *blanks       esito             1
084600100930     C                   PARM                    operazione        1
084700100930     C                   PARM      ableml        alfa64           64
084800100930     C                   IF        %error or esito = '1'
084900100930     C                   MOVEL     '1'           ERRL00
085000100930     C                   MOVEL     MSG(5)        DSMSMS
085100100930     c                   if        wricpgm <> *Blanks
085200100930     c                   Movel     '05'          s30err
085300100930     c                   Movel     dsmsms        s30msg
085400100930     c                   EndIf
085500100930     C                   ELSE
085600100930     C                   IF        esito = '2'
085700100930     C                   MOVEL     '1'           ERRL00
085800100930     C                   EVAL      DSMSMS = %TRIM(MSG(8)) + ' ' + ablsun
085900100930     c                   if        wricpgm <> *Blanks
086000100930     c                   Movel     '08'          s30err
086100100930     c                   Movel     dsmsms        s30msg
086200100930     c                   EndIf
086300100930     C                   ELSE
086400100930     C                   MOVEL     *all'0'       vsssun
086500100930     C                   MOVE      ablsun        vsssun
086600100930     C*
086700100930     C                   SELECT
086800100930     C                   WHEN      *in71
086900100930     C                   EVAL      MESS = %trim(MSG(16))
087000100930     C                   OTHER
087100100930     C                   EVAL      MESS = %trim(MSG(9)) + ablsun
087200100930     C                   ENDSL
087300100930     C*
087400100930     c                   MOVEL     MESS          s30msg
087500100930     C                   MOVEL     MESS          DSMSMS
087600100930     C                   MOVEL     '1'           CHKOK             1
087700100930     C                   ENDIF
087800100930     C                   ENDIF
087900100930     C*
088000100930     C                   ENDSR
088100100930     C*------------------------------------------------------------------------*
088200100930     C* LETabl - LEGGE IL RECORD PERL'AGGIORNAMENTO
088300100930     C*------------------------------------------------------------------------*
088400100930     C     LETabl        BEGSR
088500100930     C*
088600100930     C                   MOVEL     S30SUN        KCEsun
088700101007     C                   MOVEL     S30ISV        KCEisv
088800100930     C                   MOVEL     'SG1'         KCEtip
088900100930     C     KEYabl        CHAIN     tiabl01L                           9998
089000100930     C     *IN98         IFEQ      *ON                                          *ALLOCATO
089100100930     C                   MOVEL     '15'          S30ERR                         *ERRORE
089200100930     C                   MOVEL     MSG(15)       S30MSG                         *MESSAGGIO
089300100930     C                   ENDIF
089400100930     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
089500100930     C                   MOVEL     '14'          S30ERR                         *ERRORE
089600100930     C                   MOVEL     MSG(14)       S30MSG                         *MESSAGGIO
089700100930     C                   ENDIF
089800100930     C*
089900100930     C                   ENDSR
090000091027     C*--------------------------------------------------------------------------------------------*
090100091027     C* CARTAB - carica le tabelle occorrenti
090200091027     C*--------------------------------------------------------------------------------------------*
090300091027     C     CARTAB        BEGSR
090400091027     c*---
090500091027     c* tipi legami "internet"
090600091027     c*---
090700091027     c                   z-add     *zeros        jtle
090800091027     c                   movel     'TLE'         tbecod
090900091027     c     tbecod        setll     tntbe01l
091000091027     c     tbecod        reade     tntbe01l
091100091027     c                   dow       not %eof(tntbe01l)
091200091027     c                   if        tbeatb = *blanks
091300091027     c                   eval      dtle = tbeuni
091400091027     c* solo legami d tipo "internet"
091500091027     c                   if        §TLEGRP = 'W'
091600091027     c                   add       1             jtle
091700091027     c                   eval      sktle(jtle) = tbeke1
091800091027     c                   endif
091900091027     c                   endif
092000091027     c     tbecod        reade     tntbe01l
092100091027     c                   enddo
092200091027     C*
092300091027     C                   ENDSR
092400970526     C*------------------------------------------------------------------------*
092500970526     C* *INZSR - OPERAZIONI INIZIALI
092600970526     C*------------------------------------------------------------------------*
092700970526     C     *INZSR        BEGSR
092800970526     C*---
092900970526     C* RICEVIMENTO PARAMETRI
093000970526     C*---
093100970526     C     *ENTRY        PLIST
093200970612     C                   PARM                    KPJBA
093300050105     c                   Parm                    Tis730ds
093400050105
093500050105     c                   If        parms = 1
093600050105     c                   clear                   wricpgm           1
093700120213     c                   setoff                                       50
093800050105     c                   else
093900050105     c                   move      s30ric        wricpgm
094000120213     c                   seton                                        50
094100050105     c                   endif
094200020924     C*
094300020924     C* REPERISCE I DATI DELL'UTENTE
094400020924     C                   EXSR      REPDATIUTE
094500970526     C*
094600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
094700970526     C     XMSGPA        PLIST
094800970526     C                   PARM                    DSMSMS
094900970526     C                   PARM                    DSMSPG
095000970526     C                   PARM                    DSMSKE
095100970526     C                   PARM                    DSMSER
095200970526     C*
095300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
095400970526     C                   CALL      'XKEY'
095500970526     C                   PARM                    DSKEY
095600970526     C*---
095700970526     C* VARIABILI NON RIFERITE AL DATA BASE
095800970526     C*---
095900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
096000000714     C     *LIKE         DEFINE    s20OP0        DEPOP0
096100000714     C     *LIKE         DEFINE    s20OP1        DEPOP1
096200000714     C     *LIKE         DEFINE    s20OP0        WOP0
096300000714     C     *LIKE         DEFINE    s20OP1        WOP1
096400970526     C*---
096500970526     C* VARIABILI DI CONTROLLO
096600970526     C*---
096700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
096800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
096900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
097000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
097100970526     C*
097200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
097300000714     C                   MOVEL     'TIS730R'     DSMSPG
097400000714     C                   MOVEL     'TIS730R'     MSGQUE
097500970526     C                   MOVEL     *BLANKS       MSGKEY
097600970526     C                   MOVEL     *BLANKS       DSMSMS
097700970526     C*---
097800970526     C* VARIABILI RIFERITE AL DATA BASE
097900970526     C*---
098000991104     C     *LIKE         DEFINE    vssisv        KCEisv
098100991105     C     *LIKE         DEFINE    vsssun        KCEsun
098200000317     C     *LIKE         DEFINE    vssksu        KCEksu
098300100930     C     *LIKE         DEFINE    abltip        KCEtip
098400970526     C*---
098500970526     C* CHIAVI DI LETTURA
098600970526     C*---
098700100930     C* LETTURA tiabl01L - COMPLETA
098800100930     C     KEYabl        KLIST
098900100930     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
099000100930     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
099100100930     C                   KFLD                    KCEtip                         *TIPO SERVIZIO
099200991104     C* LETTURA tivss01L - COMPLETA
099300991104     C     KEYvss        KLIST
099400001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
099500001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
099600001116     C* LETTURA tivss02L - COMPLETA
099700001116     C     KE2vss        KLIST
099800001116     C                   KFLD                    KCEksu
099900001116     C                   KFLD                    KCEisv
100000020228     C* LETTURA tikun06L
100100020228     C     KE6kun        KLIST
100200020228     C                   KFLD                    KUNcof                         *CODICE CLI FIGLIO
100300020228     C                   KFLD                    KUNtle                         *TIPO LEGAME
100400000714     C*
100500000714     C* IMPOSTO SUBITO ALCUNE VARIABILI
100600001116     C                   EVAL      VIDisv = 'TT'
100700000721     C                   EXSR      CALORA
100800050104     C***                EVAL      DATAI = AAAA+'0101'
100900050104     C                   MOVE(P)   DATCOR        DATAI
101000060726     C                   EVAL      DATAF = '20391231'
101100020307     C*
101200020307     C* IMPOSTO QUESTE DATE CALCOLATE COME DATE D DEFAULT A VIDEO
101300020307     C                   MOVE(p)   DATAI         VIDDAI
101400020307     C                   MOVE(p)   DATAF         VIDDAF
101500000714     C*
101600000714     C* DEPOSITA LE OPZIONI DI ENTRATA
101700000714     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
101800000714     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
101900970526     C*
102000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
102100970526     C                   EXSR      IMPFUN
102200000131     C*
102300970526     C                   ENDSR
102400970526     C*------------------------------------------------------------------------*
102500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
102600970526     C*------------------------------------------------------------------------*
102700970526     C     FUNGES        BEGSR
102800970526     C*
102900970526     C* PULISCO CAMPI DI LAVORO
103000970526     C                   Z-ADD     0             W                 5 0
103100970526     C*
103200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
103300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
103400970526     C     1             DO        24            I
103500970526     C                   MOVEL     FUD(I)        FUDRIG
103600970526     C*
103700970526     C* PULIZIA FUNZIONI PRECEDENTI
103800970526     C                   MOVEL     *BLANKS       FUV(I)
103900970526     C*
104000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
104100970526     C* (SALTA F24)
104200970526     C     FUDATT        IFEQ      'S'
104300970526     C     FUDFUN        ANDNE     '24'
104400970526     C                   ADD       1             W
104500970526     C                   MOVEL     FUDDES        FUV(W)
104600970526     C                   END
104700970526     C                   END                                                    DO WHILE
104800970526     C*
104900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
105000970526     C* ALTRIMENTI DISATTIVO F24
105100970526     C     1             DO        24            I
105200970526     C                   MOVEL     FUD(I)        FUDRIG
105300970526     C*
105400970526     C     FUDFUN        IFEQ      '24'
105500970526     C     W             IFGT      6
105600970526     C                   MOVEL     'S'           FUDATT
105700970526     C                   ELSE
105800970526     C                   MOVEL     *BLANKS       FUDATT
105900970526     C                   END                                                    W GT 8
106000970526     C                   MOVEL     FUDRIG        FUD(I)
106100970526     C                   END
106200970526     C                   END                                                    DO WHILE
106300970526     C*
106400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
106500970526     C     W             DIV       6             I
106600970526     C                   MVR                     Z
106700970526     C*
106800970526     C     Z             IFNE      0
106900970526     C     I             ADD       1             Z
107000970526     C                   ELSE
107100970526     C                   Z-ADD     I             Z                 5 0
107200970526     C                   END
107300970526     C*
107400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
107500970526     C                   ADD       1             FUNGRU
107600970526     C     FUNGRU        IFGT      Z
107700970526     C     FUNGRU        ORLT      1
107800970526     C                   Z-ADD     1             FUNGRU
107900970526     C                   END
108000970526     C*
108100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
108200970526     C     FUNGRU        IFEQ      1
108300970526     C                   MOVEL     FUV(1)        FUN001
108400970526     C                   MOVEL     FUV(2)        FUN002
108500970526     C                   MOVEL     FUV(3)        FUN003
108600970526     C                   MOVEL     FUV(4)        FUN004
108700970526     C                   MOVEL     FUV(5)        FUN005
108800970526     C                   MOVEL     FUV(6)        FUN006
108900970526     C                   END
109000970526     C*
109100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
109200970526     C     FUNGRU        IFEQ      2
109300970526     C                   MOVEL     FUV(7)        FUN001
109400970526     C                   MOVEL     FUV(8)        FUN002
109500970526     C                   MOVEL     FUV(9)        FUN003
109600970526     C                   MOVEL     FUV(10)       FUN004
109700970526     C                   MOVEL     FUV(11)       FUN005
109800970526     C                   MOVEL     FUV(12)       FUN006
109900970526     C                   END
110000970526     C*
110100970526     C* TERZO GRUPPO DI 6 FUNZIONI
110200970526     C     FUNGRU        IFEQ      3
110300970526     C                   MOVEL     FUV(13)       FUN001
110400970526     C                   MOVEL     FUV(14)       FUN002
110500970526     C                   MOVEL     FUV(15)       FUN003
110600970526     C                   MOVEL     FUV(16)       FUN004
110700970526     C                   MOVEL     FUV(17)       FUN005
110800970526     C                   MOVEL     FUV(18)       FUN006
110900970526     C                   END
111000970526     C*
111100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
111200970526     C     FUNGRU        IFEQ      4
111300970526     C                   MOVEL     FUV(19)       FUN001
111400970526     C                   MOVEL     FUV(20)       FUN002
111500970526     C                   MOVEL     FUV(21)       FUN003
111600970526     C                   MOVEL     FUV(22)       FUN004
111700970526     C                   MOVEL     *BLANKS       FUN005
111800970526     C                   MOVEL     *BLANKS       FUN006
111900970526     C                   END
112000970526     C*
112100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
112200970526     C                   MOVEL     *BLANKS       FUN007
112300970526     C                   MOVEL     *BLANKS       FUN008
112400970526     C*
112500970526     C     1             DO        24            I
112600970526     C                   MOVEL     FUD(I)        FUDRIG
112700970526     C     FUDFUN        IFEQ      '24'
112800970526     C     FUDATT        ANDEQ     'S'
112900970526     C                   MOVEL     FUDDES        FUN008
113000970526     C                   END
113100970526     C                   END                                                    DO
113200970526     C*
113300970526     C                   ENDSR
113400970526     C*------------------------------------------------------------------------*
113500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
113600970526     C*------------------------------------------------------------------------*
113700970526     C     FUNCON        BEGSR
113800970526     C*
113900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
114000970526     C                   EXSR      FUNVAR
114100970526     C*
114200970526     C* PULISCO CAMPI DI LAVORO
114300970526     C                   Z-ADD     0             I                 5 0
114400970526     C                   Z-ADD     0             W                 5 0
114500970526     C*
114600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
114700970526     C     FUNKEY        IFNE      *BLANKS
114800970526     C     1             DO        24            I
114900970526     C                   MOVEL     FUD(I)        FUDRIG
115000970526     C*
115100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
115200970526     C     FUNKEY        IFEQ      FUDFUN
115300970526     C     FUDATT        ANDEQ     'S'
115400970526     C                   Z-ADD     I             W
115500970526     C                   END
115600970526     C                   END                                                    DO WHILE
115700970526     C*
115800970526     C* FUNZIONE NON ATTIVA (ERRORE)
115900970526     C     W             IFEQ      0
116000970526     C*
116100970526     C* EMISSIONE MESSAGGIO DI ERRORE
116200970526     C                   MOVEL     MSG(1)        DSMSMS
116300970526     C*
116400970526     C                   END                                                    W=0
116500970526     C                   END                                                    FUNKEY NE *BLANKS
116600970526     C*
116700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
116800970526     C     W             IFNE      0
116900970526     C     FUNKEY        OREQ      *BLANKS
117000970526     C     FUNKEY        IFEQ      *BLANKS
117100970526     C                   EXSR      ENTGES
117200970526     C                   END
117300970526     C     FUNKEY        IFEQ      '03'
117400970526     C                   EXSR      F03GES
117500970526     C                   END
117600001116     C     FUNKEY        IFEQ      '05'
117700001116     C                   EXSR      F05GES
117800001116     C                   END
117900970526     C     FUNKEY        IFEQ      '06'
118000970526     C                   EXSR      F06GES
118100970526     C                   END
118200001116     C     FUNKEY        IFEQ      '11'
118300001116     C                   EXSR      F11GES
118400001116     C                   END
118500970526     C     FUNKEY        IFEQ      '12'
118600970526     C                   EXSR      F12GES
118700970526     C                   END
118800970526     C     FUNKEY        IFEQ      '24'
118900970526     C                   EXSR      F24GES
119000970526     C                   END
119100970526     C                   END
119200970526     C*
119300970526     C                   ENDSR
119400970526     C*------------------------------------------------------------------------*
119500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
119600970526     C*------------------------------------------------------------------------*
119700970526     C     FUNVAR        BEGSR
119800970526     C*
119900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
120000970526     C*
120100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
120200970526     C     KEY           IFEQ      ENTER
120300970526     C                   MOVEL     *BLANKS       FUNKEY
120400970526     C                   END
120500970526     C     KEY           IFEQ      F1
120600970526     C                   MOVEL     '01'          FUNKEY
120700970526     C                   END
120800970526     C     KEY           IFEQ      F2
120900970526     C                   MOVEL     '02'          FUNKEY
121000970526     C                   END
121100970526     C     KEY           IFEQ      F3
121200970526     C                   MOVEL     '03'          FUNKEY
121300970526     C                   END
121400970526     C     KEY           IFEQ      F4
121500970526     C                   MOVEL     '04'          FUNKEY
121600970526     C                   END
121700970526     C     KEY           IFEQ      F5
121800970526     C                   MOVEL     '05'          FUNKEY
121900970526     C                   END
122000970526     C     KEY           IFEQ      F6
122100970526     C                   MOVEL     '06'          FUNKEY
122200970526     C                   END
122300970526     C     KEY           IFEQ      F7
122400970526     C                   MOVEL     '07'          FUNKEY
122500970526     C                   END
122600970526     C     KEY           IFEQ      F8
122700970526     C                   MOVEL     '08'          FUNKEY
122800970526     C                   END
122900970526     C     KEY           IFEQ      F9
123000970526     C                   MOVEL     '09'          FUNKEY
123100970526     C                   END
123200970526     C     KEY           IFEQ      F10
123300970526     C                   MOVEL     '10'          FUNKEY
123400970526     C                   END
123500970526     C     KEY           IFEQ      F11
123600970526     C                   MOVEL     '11'          FUNKEY
123700970526     C                   END
123800970526     C     KEY           IFEQ      F12
123900970526     C                   MOVEL     '12'          FUNKEY
124000970526     C                   END
124100970526     C     KEY           IFEQ      F13
124200970526     C                   MOVEL     '13'          FUNKEY
124300970526     C                   END
124400970526     C     KEY           IFEQ      F14
124500970526     C                   MOVEL     '14'          FUNKEY
124600970526     C                   END
124700970526     C     KEY           IFEQ      F15
124800970526     C                   MOVEL     '15'          FUNKEY
124900970526     C                   END
125000970526     C     KEY           IFEQ      F16
125100970526     C                   MOVEL     '16'          FUNKEY
125200970526     C                   END
125300970526     C     KEY           IFEQ      F17
125400970526     C                   MOVEL     '17'          FUNKEY
125500970526     C                   END
125600970526     C     KEY           IFEQ      F18
125700970526     C                   MOVEL     '18'          FUNKEY
125800970526     C                   END
125900970526     C     KEY           IFEQ      F19
126000970526     C                   MOVEL     '19'          FUNKEY
126100970526     C                   END
126200970526     C     KEY           IFEQ      F20
126300970526     C                   MOVEL     '20'          FUNKEY
126400970526     C                   END
126500970526     C     KEY           IFEQ      F21
126600970526     C                   MOVEL     '21'          FUNKEY
126700970526     C                   END
126800970526     C     KEY           IFEQ      F22
126900970526     C                   MOVEL     '22'          FUNKEY
127000970526     C                   END
127100970526     C     KEY           IFEQ      F23
127200970526     C                   MOVEL     '23'          FUNKEY
127300970526     C                   END
127400970526     C     KEY           IFEQ      F24
127500970526     C                   MOVEL     '24'          FUNKEY
127600970526     C                   END
127700970526     C     KEY           IFEQ      ROLLUP
127800970526     C                   MOVEL     'RU'          FUNKEY
127900970526     C                   END
128000970526     C     KEY           IFEQ      HELP
128100970526     C                   MOVEL     'HP'          FUNKEY
128200970526     C                   END
128300970526     C                   ENDSR
128400970526     C*------------------------------------------------------------------------*
128500970526     C* NOTE COSTRUZIONE SCHIERE
128600970526     C*------------------------------------------------------------------------*
128700970526     C*
128800970526     C* -MSG-MESSAGGI
128900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
129000970526     C* (NON TOCCARE MSG,1 E MSG,2)
129100970526     C*
129200970526     C* -CMD-COMANDI
129300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
129400970526     C*
129500970526     C* -OPD-OPZIONI
129600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
129700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
129800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
129900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
130000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
130100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
130200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
130300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
130400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
130500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
130600970526     C*
130700970526     C* -FUD-FUNZIONI
130800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
130900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
131000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
131100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
131200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
131300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
131400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
131500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
131600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
131700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
131800970526     O*------------------------------------------------------------------------*
131900970526** DEC - DECODIFICHE
132000001116  ISCRIZIONE                                                      1
132100970526** MSG - MESSAGGI
132200000714TIS730R- Codice Cliente inesistente                                            1
132300000714TIS725R- Inserimento effettuato                                                2
132400001220TIS730R- Ciente già abil. per stesso servizio con altro SGIUSR                 3
132500000714TIS730R- Campo obbligatorio                                                    4
132600000719TIS730R- Possibile errore remoto o di comunicazione                            5
132700000714TIS725R- Richiesta funzione per record inesistente                             6
132800000714TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
132900000728TIS730R- Cliente già iscritto in STRATEGI con codice:                          8
133000000717TIS730R- Iscritto ed abilitato Utente Strategi #                               9
133100001116TIS730R- Tipo servizio errato o inesistente                                    10
133200010605TIS730R- Nel periodo esistono già abilitazioni per il cliente al file TO.      11
133300010605TIS730R- Abilitazione per il cliente al file TO già esistente.                 12
133400020228TIS730R- Cliente già presente come figlio con legame WW nel periodo            13
133500100930TIS730R- Richiesta funzione per record inesistente                             14
133600100930TIS730R- Record momentaneamente allocato: riprovare più tardi                  15
133700100930TIS730R- Richiesta invio nuova password effettuato.                            16
133800100930TIS730R-                                                                       17
133900100930TIS730R-                                                                       18
134000100930TIS730R-                                                                       19
134100100930TIS730R-                                                                       20
134200970526** FUD - FUNZIONI
134300970526                   -        (S/N)=N                        funz.01¬
134400970526                   -        (S/N)=N                        funz.02¬
134500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
134600970526                   -        (S/N)=N                        funz.04¬
134700001116F05=Refresh        -        (S/N)=S                        funz.05¬
134800970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
134900970527                   -        (S/N)=N                        funz.07¬
135000970527                   -        (S/N)=N                        funz.08¬
135100970527                   -        (S/N)=N                        funz.09¬
135200970527                   -        (S/N)=N                        funz.10¬
135300050105F11=Abilit. servizi-        (S/N)=S                        funz.11¬
135400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
135500970526                   -        (S/N)=N                        funz.13¬
135600970526                   -        (S/N)=N                        funz.14¬
135700970526                   -        (S/N)=N                        funz.15¬
135800970526                   -        (S/N)=N                        funz.16¬
135900970526                   -        (S/N)=N                        funz.17¬
136000970526                   -        (S/N)=N                        funz.18¬
136100001116                   -        (S/N)=N                        funz.19¬
136200970526                   -        (S/N)=N                        funz.20¬
136300970526                   -        (S/N)=N                        funz.21¬
136400970526                   -        (S/N)=N                        funz.22¬
136500970526                   -        (S/N)=N                        funz.23¬
136600050105F24=Altri tasti    -        (S/N)=N                        funz.24¬
