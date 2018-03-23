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
012300050105     d Tis730ds      e ds
012400050105
012500050105     D                SDS
012600050105     D  PARMS                 37     39  0
012700100930
012800100930     D/COPY GAITRASRC/SRCPROTOPR,UBRANDOM
012900100930     D/COPY GAITRASRC/SRCPROTOPI,UBRANDOM
013000100930
013100000714
013200970526     C*------------------------------------------------------------------------*
013300970526     C* MAIN LINES
013400970526     C*------------------------------------------------------------------------*
013500100930      *
013600100930     c                   setoff                                       71
013700110112     c                   Movel     *blanks       ablsun
013800100930      *
013900050105      * se pgm richiamato vado subito alla routine richiamata dal tasto funzione F6
014000050105     c                   If        wricpgm <> *Blanks
014100100930     c                   select
014200100930     c* - inserimento
014300100930     c                   when      S30OPZ = 'II'
014400050105     c                   ExSr      movs30
014500050105     c                   ExSr      inseri
014600101018     c* - modifica
014700101018     c                   when      S30OPZ = 'UU'
014800100930     c                   seton                                        71
014900100930     c                   ExSr      movs30
015000101018     c                   ExSr      modifi
015100100930     c                   endsl
015200100930     c*
015300050111     c                   Goto      fine
015400050105     c                   EndIf
015500970526     C*
015600970526     C* NON CI SONO ERRORI IN ENTRATA
015700001116     C     ERRL00        IFEQ      '0'
015800970526     C*
015900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016000001116     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016300970526     C*
016400970526     C* EMETTE FORMATO VIDEO
016500970526     C                   EXSR      BUTFMR
016600970526     C*
016700970526     C* GESTIONE TASTI FUNZIONALI
016800970526     C                   EXSR      FUNCON
016900001116     C                   ENDDO
017000970526     C*
017100970526     C* CI SONO ERRORI IN ENTRATA
017200001116     C                   ELSE
017300991104     C                   MOVEL     '1'           s20err
017400991104     C                   MOVEL     DSMSMS        s20msg
017500001116     C                   ENDIF
017600970526     C* FINE PGM
017700050111     c     fine          tag
017800001116     C                   EXSR      FINSR
017900001116     C*
018000970526     C                   SETON                                        LR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     BUTFMR        BEGSR
018500970526     C*
018600970526     C* EMETTE LA TESTATA
018700000714     C                   WRITE     S730D01
018800970526     C*
018900970526     C* SPEDISCE EVENTUALE MESSAGGIO
019000970526     C                   EXSR      SNDMSG
019100970526     C*
019200970526     C* EMETTE LE FUNZIONI
019300000714     C                   WRITE     S730D02
019400970526     C*
019500970526     C* EMETTE IL VIDEO
019600000714     C                   EXFMT     S730D03
019700970526     C*
019800970526     C* REIMPOSTA FLAG DI ERRORE
019900970526     C                   MOVEL     '0'           ERRL00
020000970526     C*
020100970526     C* REIMPOSTA INDICATORI DI ERRORE
020200000714     C                   SETOFF                                       616263
020300010227     C                   SETOFF                                       6465
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     SNDMSG        BEGSR
021000970526     C*
021100970526     C* PULISCE IL SFILE MESSAGGI
021200970526     C                   MOVEL     '1'           *IN40
021300000714     C                   WRITE     S730DMC
021400970526     C                   MOVEL     '0'           *IN40
021500970526     C*
021600970526     C* VISUALIZZA IL MESSAGGIO
021700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
021800970526     C                   CALL      'XMSG'        XMSGPA
021900001116     C     DSMSER        IFEQ      *ZEROS
022000970526     C                   MOVEL     DSMSKE        MSGKEY
022100001116     C                   ENDIF
022200970526     C                   Z-ADD     1             MSGULT
022300000714     C                   WRITE     S730DMS
022400000714     C                   WRITE     S730DMC
022500970526     C*
022600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022700970526     C                   MOVEL     *BLANKS       DSMSMS
022800970526     C*
022900970526     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* ENTGES - GESTIONE ENTER -> INVIO
023200970526     C*------------------------------------------------------------------------*
023300970526     C     ENTGES        BEGSR
023400970526     C*
023500970526     C* CONTROLLA I CAMPI IMMESSI
023600970526     C                   EXSR      DECODI
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F03GES - GESTIONE F03 -> FINE
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F03GES        BEGSR
024300970526     C*
024400970526     C* IMPOSTA "PREMUTO F03"
024500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
024600991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
024700970526     C*
024800970526     C                   ENDSR
024900001116     C*------------------------------------------------------------------------*
025000001116     C* F05GES - GESTIONE F05 -> REFRESH
025100001116     C*------------------------------------------------------------------------*
025200001116     C     F05GES        BEGSR
025300001116     C*
025400001116     C* LANCIA LA ROUTINE DI AZZERAMENTO VIEO
025500001116     C                   EXSR      AZZVID
025600001116     C*
025700001116     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900010605     C* F06GES - GESTIONE F06 -> INSERIMENTO
026000970526     C*------------------------------------------------------------------------*
026100970526     C     F06GES        BEGSR
026200970526     C*
026300970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
026400970526     C                   EXSR      INSERI
026500010605     C*
026600010605     C* LANCIA LA ROUTINE DI AZZERAMENTO VIDEO
026700010605     C                   EXSR      AZZVID
026800001116     C*
026900970526     C                   ENDSR
027000001116     C*------------------------------------------------------------------------*
027100001116     C* F11GES - GESTIONE F11 -> PASSAGGIO A GESTIONE ABILITAZIONE AI SERVIZI
027200001116     C*------------------------------------------------------------------------*
027300001116     C     F11GES        BEGSR
027400001116     C*
027500001116     C* PASSA ALLA GESTIONE ABILITAZIONI
027600001116     C                   CLEAR                   tis720
027700001116     C                   MOVEL     'P02'         s20OP0                         *LIVELLO PROCEDURA
027800001116     C                   MOVEL     'O09'         s20OP1                         *LIVELLO PGM
027900001116     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
028000001116     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
028100001116     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
028200001116     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
028300001116     C                   MOVEL     'WW'          s20tle                         *TIPO LEGAME
028400001116     C                   MOVE(p)   VIDksu        s20ksc                         *CLIENTE UNIFICANTE
028500001116     C*
028600001116     C                   MOVEL     tis720        KPJBU
028700001116     C                   CALL      'TIS724R'
028800001116     C                   PARM                    KPJBA
028900001116     C*
029000001116     C* LANCIA LA ROUTINE DI AZZERAMENTO VIEO
029100001116     C                   EXSR      AZZVID
029200001116     C*
029300001116     C                   ENDSR
029400970526     C*------------------------------------------------------------------------*
029500970526     C* F12GES - GESTIONE F12 -> RITORNO
029600970526     C*------------------------------------------------------------------------*
029700970526     C     F12GES        BEGSR
029800970526     C*
029900970526     C* IMPOSTA "PREMUTO F12"
030000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
030100991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
030200970526     C*
030300970526     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
030600970526     C*------------------------------------------------------------------------*
030700970526     C     F24GES        BEGSR
030800970526     C*
030900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
031400970526     C*------------------------------------------------------------------------*
031500970526     C     INSERI        BEGSR
031600970526     C*
031700970526     C* CONTROLLA I CAMPI IMMESSI
031800050105      * se non è richiamato
031900050105     c                   if        wricpgm = *Blanks
032000970526     C                   EXSR      DECODI
032100050105     c                   EndIf
032200970526     C*
032300000714     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400000719     C* IMPOSTA I CAMPI DEL RECORD "VSS" DA SCRIVERE
032500000719     C                   CLEAR                   tivss000
032600000714     C                   MOVEL     VIDksu        vssksu
032700001116     C                   MOVEL     VIDisv        vssisv
032800020307     C                   MOVE(p)   VIDDAI        vssdde
032900020307     C                   MOVE(p)   VIDDAF        vssdsc
033000000714     C                   Z-ADD     *ZEROS        vssdtr                         *DATA TRASMISSIONE
033100000714     C                   MOVEL     *BLANKS       vssftr                         *FLAG TRASMISSIONE
033200100930     C* IMPOSTA I CAMPI DEL RECORD "ABL" DA SCRIVERE
033300000719     C                   CLEAR                   tiabl000
033400100930     C                   EXSR      crtpwd
033500100930     C                   EVAL      operazione = 'I'
033600100930     C                   EXSR      EXESGI
033700100930     C*
033800000714     C* SOLO SE TUTTO OK
033900010605     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034000001116     C                   WRITE     tivss000
034100001116     C                   WRITE     tiabl000
034200001116     C                   ENDIF
034300050105     C                   ENDIF
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700001116     C* DECODI - CONTROLLA CAMPI E DECODIFICA
034800970526     C*------------------------------------------------------------------------*
034900970526     C     DECODI        BEGSR
035000000714     C*
035100000714     C                   SETOFF                                       616263
035200010227     C                   SETOFF                                       6465
035300001116     C*
035400970528     C*--------------------
035500000714     C* CLIENTE
035600970528     C*--------------------
035700991105     C     *LIKE         DEFINE    vssksu        ksuVID
035800001116     C     ERRL00        IFEQ      '0'
035900991105     C                   MOVEL     *BLANKS       VDDksu
036000001116     C     VIDksu        IFEQ      *blanks                                      *STANDARD
036100001116     C     VIDksu        OREQ      *zeros                                       *STANDARD
036200000705     C                   MOVEL     '1'           ERRL00
036300000705     C                   SETON                                        61
036400000714     C                   MOVEL     MSG(4)        DSMSMS
036500991105     C                   MOVEL     *ZEROS        ksuVID
036600001116     C                   ELSE
036700041230     C     VIDksu        IFGT      '09999999'                                   *STANDARD
036800041230     C                   MOVEL     '1'           ERRL00
036900041230     C                   SETON                                        61
037000041230     C                   MOVEL     MSG(1)        DSMSMS
037100041230     C                   MOVEL     *ZEROS        ksuVID
037200041230     C                   ELSE
037300991105     C                   MOVEL     VIDksu        Wksu
037400991105     C                   MOVEL     *blank        Wdksu
037500991105     C                   EXSR      CHKksu
037600991105     C                   MOVEL     Wksu          VIDksu
037700991105     C                   MOVEL     WDksu         VDDksu
037800991105     C                   MOVEL     VIDksu        ksuVID
037900001116     C     ERRL00        IFEQ      '1'
038000970528     C                   SETON                                        61
038100000714     C                   MOVEL     MSG(1)        DSMSMS
038200020228     C                   ELSE
038300091027     C* Che nn sia cliente figlio
038400020228     C                   MOVE(p)   VIDksu        KUNcof
038500091027     c*
038600091027     c* ciclo x tutti i legami d tipo "internet"
038700091027     c                   z-add     1             jtle
038800091027     c                   dow       jtle <= %elem(sktle)
038900091027     c                   if        sktle(jtle) <> *blanks
039000091027     c*
039100091027     C                   movel     sktle(jtle)   kuntle
039200020228     C     KE6kun        SETLL     tikun06l
039300020228     C     KE6kun        READE     tikun06l
039400020228     C                   DOW       not %eof(tikun06l)
039500020228     C     KUNvat        IFEQ      *blanks                                      *annullati/sospesi
039600020307     C                   IF        KUNdsc <  VIDDAI  OR                         *non valido
039700020307     C                             KUNdde >  VIDDAF
039800020228     C                   ELSE                                                   *valido
039900020409     C                   MOVEL     '1'           ERRL00
040000020228     C                   SETON                                        61
040100020228     C                   MOVEL     MSG(13)       DSMSMS
040200020228     C                   LEAVE
040300020228     C                   ENDIF
040400020228     C                   ENDIF
040500020228     C     KE6kun        READE     tikun06l
040600020228     C                   ENDDO
040700091027     c*
040800091027     c                   else
040900091027     c                   leave
041000091027     c                   endif
041100091027     c*
041200091027     c                   add       1             jtle
041300091027     c                   enddo
041400091027     c*
041500001116     C                   ENDIF
041600001116     C                   ENDIF
041700001116     C                   ENDIF
041800041230     C                   ENDIF
041900000714     C*
042000000714     C*--------------------
042100000714     C* RESPONSABILE
042200000714     C*--------------------
042300001116     C     ERRL00        IFEQ      '0'
042400000714     C     VIDrsp        IFEQ      *blanks
042500000714     C                   MOVEL     '1'           ERRL00
042600000714     C                   SETON                                        62
042700000714     C                   MOVEL     MSG(4)        DSMSMS
042800000714     C                   ENDIF
042900000714     C                   ENDIF
043000000714     C*
043100000714     C*--------------------
043200000714     C* E-MAIL RESPONSABILE
043300000714     C*--------------------
043400001116     C     ERRL00        IFEQ      '0'
043500000714     C     VIDem1        IFEQ      *blanks
043600000714     C     VIDem2        ANDEQ     *blanks
043700000714     C                   MOVEL     '1'           ERRL00
043800000714     C                   SETON                                        63
043900001116     C                   MOVEL     MSG(4)        DSMSMS
044000010115     C                   ELSE
044100010115     C* Lancio utility di controllo formale indirizzo e-mail
044200010115     C                   CLEAR                   DSEMAIL
044300010115     C                   EVAL      §EMLINDI = VIDem1+VIDem2
044400010115     C                   CALL      'XEMAIL'
044500010115     C                   PARM                    DSEMAIL
044600010115     C                   IF        §EMLERRO = '0'
044700010115     C                   EVAL      VIDem1 = %subst(§EMLINDO:1:49)
044800010115     C                   EVAL      VIDem2 = %subst(§EMLINDO:50:49)
044900010115     C                   ELSE
045000010115     C                   SETON                                        63
045100010115     C                   MOVEL     §EMLMSGO      DSMSMS
045200010115     C                   MOVEL     '1'           ERRL00
045300010115     C                   ENDIF
045400010115     C                   ENDIF
045500010115     C                   ENDIF
045600001116     C*
045700001116     C*--------------------
045800001116     C* TIPO SERVIZIO
045900001116     C*--------------------
046000001116     C     ERRL00        IFEQ      '0'
046100001116     C     VIDisv        IFNE      *blank
046200001116     C                   EXSR      CNTISV
046300001116     C                   ELSE
046400041230     C                   MOVEL     '1'           ERRL00
046500041230     C                   SETON                                        64
046600041230     C                   MOVEL     MSG(4)        DSMSMS
046700001116     C                   MOVEL     *blanks       VDDisv
046800001116     C                   ENDIF
046900001116     C                   ENDIF
047000001116     C*
047100001116     C*--------------------
047200001116     C* CONTROLLO VALIDA AI FINI DELL'INSERIMENTO DELL'ABILITAZIONE AL SERVIZIO RICHIESTO
047300001116     C*--------------------
047400001116     C     ERRL00        IFEQ      '0'
047500001116     C                   EVAL      vssok = *off
047600001116     C                   EVAL      KCEksu = VIDksu
047700001116     C                   EVAL      KCEisv = VIDisv
047800001116     C     KE2vss        SETLL     tivss02l
047900001116     C     KE2vss        READE     tivss02l
048000001116     C                   DOW       not %eof
048100001116     C                   IF        vssdde <= DATCOR and
048200001116     C                             vssdsc >= DATCOR and
048300001116     C                             vssvat <> 'A'    and
048400001116     C                             vssvat <> 'S'
048500001116     C                   EVAL      vssok = *on
048600001116     C                   LEAVE
048700001116     C                   ENDIF
048800001116     C     KE2vss        READE     tivss02l
048900001116     C                   ENDDO
049000001116     C*
049100001116     C                   IF        vssok = *on
049200001116     C                   MOVEL     '1'           ERRL00
049300001116     C                   SETON                                        6164
049400001220     C                   EVAL      DSMSMS = %TRIM(MSG(3)) + ' ' + ablsun
049500001116     C                   ENDIF
049600001116     C                   ENDIF
049700001116     C*
049800000714     C                   ENDSR
049900970528     C*------------------------------------------------------------------------*
050000991105     C* CHKksu - CONTROLLA codice padre figlio
050100970528     C*------------------------------------------------------------------------*
050200991105     C     CHKksu        BEGSR
050300970528     C*
050400991105     C     *LIKE         DEFINE    VIDksu        Wksu
050500991105     C     *LIKE         DEFINE    VDDksu        WDksu
050600000714     C                   MOVE      *zeros        wksc
050700970528     C*
050800970528     C* RICHIESTO ELENCO
050900970528     C                   SETOFF                                           28
051000991105     C     '?'           SCAN      Wksu                                   28
051100001116     C     *IN28         IFEQ      *ON
051200000714     C* RICHIESTO ELENCO CODICI CLIENTI
051300991008     C                   CLEAR                   DSXC
051400020924     C                   Z-ADD     DUTKCI        DXCCAP
051500991008     C                   CALL      'XCLIR'
051600991008     C                   PARM                    DSXC
051700991008     C* SCOMPATTA
051800991008     C                   MOVEA     DXCKSC        KSA                        ALF
051900001116     C     1             DO        30            J                 3 0
052000991008     C                   MOVE      KSA(J)        DSKSA                      F
052100991008     C                   Z-ADD     DSKSN         KSC(J)                     M
052200001116     C                   ENDDO
052300991011     C                   MOVEL     KSC(1)        WKSC              7
052400991011     C                   MOVEL     '1'           $TESVD            1        TA
052500001116     C                   ENDIF
052600991008     C*
052700991008     C* CODICE CLIENTE NUMERICO
052800001116     C     WKSC          IFLT      *ZEROS
052900991011     C                   MOVEL     '1'           WERR              1
053000001116     C                   ELSE
053100970528     C*
053200970528     C* SOLO CARATTERI NUMERICI
053300000714     C   28              MOVE      *all'0'       wksu
053400000714     C   28              MOVE      wksc          wksu
053500970528     C                   SETOFF                                       28
053600991105     C                   TESTN                   Wksu                 28
053700001116     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
053800970528     C                   MOVEL     '1'           ERRL00
053900001116     C                   ELSE                                                   *E'NUMERO DA TESTN
054000991105     C                   MOVE      Wksu          W001              1
054100001116     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
054200970528     C                   MOVEL     '1'           ERRL00
054300001116     C                   ELSE                                                   *NUMERO
054400970528     C*
054500970528     C* CONTROLLA ESISTENZA CODICE
054600991210     C                   MOVE      Wksu          wcli              7 0
054700000714     C                   EXSR      DECTIB
054800001116     C                   ENDIF
054900001116     C                   ENDIF
055000001116     C                   ENDIF
055100970528     C*
055200970528     C                   ENDSR
055300001116     C*------------------------------------------------------------------------*
055400001116     C* CNTISV - CONTROLLA tipo servizio a cui il cliente deve essere abilitato
055500001116     C*------------------------------------------------------------------------*
055600001116     C     cntisv        begsr
055700001116     C                   CLEAR                   TIBS02DS
055800001116     C*
055900001116     C     '?'           SCAN      VIDisv                                 30
056000001116     C*  Ricerca
056100001116     C                   EVAL      T02TLA = 'L'
056200001116     C                   IF        *IN30 = *ON
056300001116     C                   MOVEL     'R'           T02MOD
056400001116     C                   ELSE
056500001116     C                   MOVEL     'C'           T02MOD
056600001116     C                   MOVEL(P)  VIDisv        T02KE1
056700001116     C                   ENDIF
056800001116     C                   MOVEL     'ISV'         T02COD
056900001116     C                   MOVEL     KNSIF         T02SIF
057000001116     C                   CALL      'TIBS02R'
057100001116     C                   PARM                    KPJBA
057200001116     C                   PARM                    TIBS02DS
057300001116     C
057400001116     C*
057500001116     C                   IF        T02ERR = *blanks
057600001116     C                   MOVEL     T02KE1        VIDisv
057700001116     C                   MOVEL     T02UNi        VDDisv
057800010227     C                   IF        VIDisv = 'TD'
057900010227     C                   SETON                                        65
058000010227     C                   ENDIF
058100001116     C                   ELSE
058200001116     C                   MOVEL     '1'           ERRL00
058300001116     C                   SETON                                        64
058400001116     C                   MOVEL     MSG(10)       DSMSMS
058500001116     C                   ENDIF
058600001116     C
058700001116     C                   endsr
058800991210     C*------------------------------------------------------------------------*
058900000714     C* DECTIB - Reperimento RAgione Sociale del Cliente
059000991210     C*------------------------------------------------------------------------*
059100000714     C     DECTIB        BEGSR
059200000714     C*
059300991210     C                   CLEAR                   BS69DS
059400991210     C                   CLEAR                   ACODS
059500991210     C                   MOVEL     KNSIF         I69SIF
059600991210     C                   Z-ADD     WCLI          I69KAC
059700991210     C                   CALL      'TIBS69R'
059800991210     C                   PARM                    BS69DS
059900991210     C                   PARM                    ACODS
060000991210     C                   PARM                    INDDS
060100991210     C                   PARM                    CLPDS
060200991210     C                   PARM                    CLSDS
060300991210     C     O69ERR        IFNE      '1'
060400991210     C                   MOVEL     ACORAG        wdksu
060500991210     C                   ELSE
060600991210     C                   MOVEL     *ALL'*'       Wdksu
060700000714     C                   MOVE      '1'           ERRL00
060800991210     C                   ENDIF
060900000714     C*
061000000714     C                   ENDSR
061100991210     C*------------------------------------------------------------------------*
061200991210     C* FINSR - OPERAZIONI FINALI
061300991210     C*------------------------------------------------------------------------*
061400991210     C     FINSR         BEGSR
061500991210     C*
061600991210     C* CHIUDE I FILE DEI PGM CHIAMATI
061700991210     C                   CLEAR                   bs69ds
061800991210     C                   CLEAR                   acods
061900991210     C                   CLEAR                   indds
062000991210     C                   CLEAR                   clpds
062100991210     C                   CLEAR                   clsds
062200991210     C                   MOVEL     'C'           i69tla
062300991210     C                   MOVEL     knsif         i69sif
062400991210     C                   CALL      'TIBS69R'
062500991210     C                   PARM                    bs69ds
062600991210     C                   PARM                    acods
062700991210     C                   PARM                    indds
062800991210     C                   PARM                    clpds
062900991210     C                   PARM                    clsds
063000991210     C*
063100991210     C                   ENDSR
063200970526     C*------------------------------------------------------------------------*
063300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
063400970526     C*------------------------------------------------------------------------*
063500970526     C     AZZVID        BEGSR
063600970526     C* CAMPI
063700000714     C                   MOVEL     *BLANKS       VIDksu
063800000714     C                   MOVEL     *BLANKS       VIDrsp
063900000714     C                   MOVEL     *BLANKS       VIDem1
064000000714     C                   MOVEL     *BLANKS       VIDem2
064100001116     C                   MOVEL     'TT'          VIDisv
064200010306     C                   MOVEL     *BLANKS       VIDtel
064300010306     C                   MOVEL     *BLANKS       VIDfax
064400010306     C                   MOVEL     *BLANKS       VIDfax
064500970526     C* DECODIFICHE
064600000714     C                   MOVEL     *BLANKS       VDDksu
064700001116     C                   MOVEL     *BLANKS       VDDisv
064800001116     C* VARIE
064900001116     C                   MOVEL     '0'           CHKOK
065000970526     C*
065100970526     C                   ENDSR
065200970526     C*------------------------------------------------------------------------*
065300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
065400970526     C*------------------------------------------------------------------------*
065500970526     C     IMPFUN        BEGSR
065600001116     C*
065700001116     C* SE OK INSERIMENTO ABILITA FUNZIONE DI "GESTIONE ABILITAZIONI"
065800050105     C*    CHKOK         IFNE      '1'
065900050105     C*                  MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
066000050105     C*                  MOVEL     'N'           FUDATT
066100050105     C*                  MOVEL     FUDRIG        FUD(11)
066200050105     C*                  ELSE
066300050105     C*                  MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
066400050105     C*                  MOVEL     'S'           FUDATT
066500050105     C*                  MOVEL     FUDRIG        FUD(11)
066600050105     C*                  ENDIF
066700050111      * disabilito il tasto funzione F11 se pgm richiamato
066800050111     c                   If        wricpgm <> *Blanks
066900050111     C                   MOVEL     FUD(11)       FUDRIG                         *GEST. ABILITAZ.
067000050111     C                   MOVEL     'N'           FUDATT
067100050111     C                   MOVEL     FUDRIG        FUD(11)
067200050111     c                   EndIf
067300001116     C*
067400001116     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
067500001116     C     DEPOP1        IFEQ      'O05'
067600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
067700970526     C                   MOVEL     'N'           FUDATT
067800970526     C                   MOVEL     FUDRIG        FUD(6)
067900001116     C                   ENDIF
068000970526     C*
068100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
068200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
068300970526     C                   EXSR      FUNGES
068400970526     C*
068500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
068600970526     C                   MOVEL     DEC(1)        VIDFUN
068700970526     C*
068800970526     C                   ENDSR
068900000717     C*------------------------------------------------------------------------*
069000000717     C* CRTPWD - Creazione Password Cliente 1° accesso
069100000717     C*------------------------------------------------------------------------*
069200000717     C     CRTPWD        BEGSR
069300000717     C*
069400000717     C                   MOVEA     VDDKSU        PASS
069500000721     C                   EXSR      CALORA
069600000721     C                   MOVE(p)   ORA           Alfa4             4
069700000717     C                   MOVEL     *blanks       Alfa1             1
069800100930     C                   MOVEL     *blanks       AlfaRnd           1
069900000717     C                   Z-ADD     *zeros        Num1              1 0
070000101007     C                   EVAL      ablpwd = *blanks
070100000717     C     1             DO        30            x                 2 0
070200000717     C                   EVAL      Alfa1  = PASS(x)
070300000719     C                   Z-ADD     *zeros        pos
070400000719     C                   EVAL      pos = %scan(PASS(x):alfabeto)
070500000719     C                   IF        pos > 0 AND Num1 < 3
070600000717     C                   EVAL      ablpwd = %trim(ablpwd) + Alfa1
070700000717     C                   ADD       1             Num1
070800000717     C                   ENDIF
070900000717     C                   ENDDO
071000100930     C                   EVAL      ablpwd = %trim(ablpwd) + Alfa4
071100100930     C*
071200100930     C* Aggiungo x sicurezza 1 byte "random"
071300100930     C                   EVAL      pInValDa   = 1
071400100930     C                   EVAL      pInValA    = 9
071500100930     C                   EVAL      pOutRandom = *zeros
071600100930     C                   CALLP(e)  UBRANDOM_Retrieve(pInValDa :
071700100930     C                                               pInValA  :
071800100930     C                                               pOutRandom)
071900100930     C*
072000100930     C                   IF        not %error
072100100930     C                   EVAL      AlfaRnd = %trim(%char(pOutRandom))
072200100930     C                   EVAL      ablpwd = %trim(ablpwd) + AlfaRnd
072300100930     C                   ENDIF
072400000717     C*
072500000717     C                   ENDSR
072600000719     C*------------------------------------------------------------------------*
072700000719     C* CALORA - Reperimento data ora di sistema in corso di esecuzione
072800000719     C*------------------------------------------------------------------------*
072900000719     C     CALORA        BEGSR
073000000719     C*
073100000719     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
073200000719     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
073300000719     C                   MOVEL     WN14          ORA               6            *ORA
073400000719     C                   Z-ADD     WN8           G08DAT
073500000719     C                   Z-ADD     *ZEROS        G08INV
073600000719     C                   MOVEL     '0'           G08ERR
073700000719     C                   CALL      'XSRDA8'
073800000719     C                   PARM                    WLBDA8
073900000719     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
074000000719     C                   MOVEL     DATCOR        AAAA              4            *ANNO ALFA
074100000719     C*
074200000719     C                   ENDSR
074300020924     C*--------------------------------------------------------------------------------------------*
074400020924     C* REPERISCE I DATI UTENTE
074500020924     C*--------------------------------------------------------------------------------------------*
074600020924     C     REPDATIUTE    BEGSR
074700020924     C*
074800020924     C* INIZIALIZZA VARIABILI DI WRK
074900020924     C                   CLEAR                   TIBS34DS
075000020924     C                   CLEAR                   AZUTEDS
075100020924     C                   CLEAR                   DDATIUTE
075200020924     C*
075300020924     C     *DTAARA       DEFINE    §azute        azuteds
075400020924     C     *DTAARA       DEFINE    §datiute      ddatiute
075500020924     C                   IN(E)     *DTAARA
075600020924if  1C                   IF        %Error
075700020924     c                   EVAL      I34Tla = 'L'
075800020924     C                   CALL      'TIBS34R'
075900020924     C                   PARM                    Tibs34Ds
076000020924     C                   IN        *DTAARA
076100020924e   1C                   ENDIF
076200020924     C*
076300020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
076400020924     C                   MOVEL(P)  RSUT          DSFIRS           20
076500020924     C*
076600020924     C                   ENDSR
076700050105      *------------------------------------------------------------------------*
076800050105      * MOVS30 - Muovo i campi della ds esterna nei campi video
076900050105      *------------------------------------------------------------------------*
077000050105     c     Movs30        BegSr
077100050105
077200050105     c                   Move      s30ksu        vidksu
077300050105     c                   Eval      vidrsp = s30rsp
077400050105     c                   Eval      videm1 = %subst(s30eml:1:49)
077500050105     c                   Eval      videm2 = %subst(s30eml:50:49)
077600050105     c                   Eval      vidisv = s30isv
077700050105     c                   Eval      viddai = s30dti
077800050105     c                   Eval      viddaf = s30dtf
077900050105     c                   Eval      viddep = s30dep
078000110112     c                   Eval      ablsun = s30sun
078100050105     c                   Clear                   vidtel
078200050105     c                   Clear                   vidfax
078300050105     c                   Move      vidksu        wcli
078400050105     c                   ExSr      Dectib
078500050105     c                   Move      wdksu         vddksu
078600091027     c*
078700050105     c                   EndSr
078800100930     C*------------------------------------------------------------------------*
078900101018     C* MODIFI - OPERAZIONI DI MODIFICA
079000100930     C*------------------------------------------------------------------------*
079100101018     C     MODIFI        BEGSR
079200100930     C*
079300100930     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
079400100930     C                   EXSR      LETabl
079500100930     C*
079600100930     C* SE TUTTO OK => PROCEDO
079700100930     C                   IF        S30ERR = *blanks
079800100930     C                   IF        S30PWD = *blanks
079900100930     C                   EXSR      crtpwd
080000100930     C                   ELSE
080100100930     C                   EVAL      ablpwd = S30PWD
080200100930     C                   ENDIF
080300100930     C*
080400100930     C                   EVAL      operazione = 'A'
080500100930     C                   EXSR      EXESGI
080600100930     C*
080700100930     C* SOLO SE TUTTO OK
080800100930     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
080900100930     C                   UPDATE    tiabl000
081000100930     C                   ENDIF
081100100930     C                   ENDIF
081200100930     C*
081300100930     C                   ENDSR
081400100930     C*------------------------------------------------------------------------*
081500100930     C* EXESGI - OPERAZIONI "LATO SGI"
081600100930     C*------------------------------------------------------------------------*
081700100930     C     EXESGI        BEGSR
081800100930     C*
081900100930     C                   MOVE      VIDksu        ablute
082000100930     C                   EVAL      ablisv = VIDisv
082100100930     C                   EVAL      ableml = %trim(VIDem1) + %trim(VIDem2)
082200100930     C                   EVAL      ablrsp = VIDrsp
082300100930     C                   EVAL      abltip = 'SG1'
082400100930     C                   EVAL      ablinv = '0'
082500100930     C                   EVAL      abltel = VIDtel
082600100930     C                   EVAL      ablfax = VIDfax
082700100930     C                   EVAL      abldep = VIDdep
082800100930     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
082900100930     C                   EVAL      Alfa30 = ablrsp
083000100930     C     api:' '       XLATE     Alfa30        Alfa30
083100100930     C*
083200100930     C                   EVAL      Alfa20 = VDDksu
083300100930     C     api:' '       XLATE     Alfa20        Alfa20
083400100930     C*
083500100930     C                   CALL(e)   'TIS730C1'
083600100930     C                   PARM                    ablute
083700100930     C                   PARM                    ablpwd
083800100930     C                   PARM                    alfa20           20
083900100930     C                   PARM                    alfa30           30
084000110112     C                   PARM                    ablsun
084100100930     C                   PARM      *blanks       esito             1
084200100930     C                   PARM                    operazione        1
084300100930     C                   PARM      ableml        alfa64           64
084400100930     C                   IF        %error or esito = '1'
084500100930     C                   MOVEL     '1'           ERRL00
084600100930     C                   MOVEL     MSG(5)        DSMSMS
084700100930     c                   if        wricpgm <> *Blanks
084800100930     c                   Movel     '05'          s30err
084900100930     c                   Movel     dsmsms        s30msg
085000100930     c                   EndIf
085100100930     C                   ELSE
085200100930     C                   IF        esito = '2'
085300100930     C                   MOVEL     '1'           ERRL00
085400100930     C                   EVAL      DSMSMS = %TRIM(MSG(8)) + ' ' + ablsun
085500100930     c                   if        wricpgm <> *Blanks
085600100930     c                   Movel     '08'          s30err
085700100930     c                   Movel     dsmsms        s30msg
085800100930     c                   EndIf
085900100930     C                   ELSE
086000100930     C                   MOVEL     *all'0'       vsssun
086100100930     C                   MOVE      ablsun        vsssun
086200100930     C*
086300100930     C                   SELECT
086400100930     C                   WHEN      *in71
086500100930     C                   EVAL      MESS = %trim(MSG(16))
086600100930     C                   OTHER
086700100930     C                   EVAL      MESS = %trim(MSG(9)) + ablsun
086800100930     C                   ENDSL
086900100930     C*
087000100930     c                   MOVEL     MESS          s30msg
087100100930     C                   MOVEL     MESS          DSMSMS
087200100930     C                   MOVEL     '1'           CHKOK             1
087300100930     C                   ENDIF
087400100930     C                   ENDIF
087500100930     C*
087600100930     C                   ENDSR
087700100930     C*------------------------------------------------------------------------*
087800100930     C* LETabl - LEGGE IL RECORD PERL'AGGIORNAMENTO
087900100930     C*------------------------------------------------------------------------*
088000100930     C     LETabl        BEGSR
088100100930     C*
088200100930     C                   MOVEL     S30SUN        KCEsun
088300101007     C                   MOVEL     S30ISV        KCEisv
088400100930     C                   MOVEL     'SG1'         KCEtip
088500100930     C     KEYabl        CHAIN     tiabl01L                           9998
088600100930     C     *IN98         IFEQ      *ON                                          *ALLOCATO
088700100930     C                   MOVEL     '15'          S30ERR                         *ERRORE
088800100930     C                   MOVEL     MSG(15)       S30MSG                         *MESSAGGIO
088900100930     C                   ENDIF
089000100930     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
089100100930     C                   MOVEL     '14'          S30ERR                         *ERRORE
089200100930     C                   MOVEL     MSG(14)       S30MSG                         *MESSAGGIO
089300100930     C                   ENDIF
089400100930     C*
089500100930     C                   ENDSR
089600091027     C*--------------------------------------------------------------------------------------------*
089700091027     C* CARTAB - carica le tabelle occorrenti
089800091027     C*--------------------------------------------------------------------------------------------*
089900091027     C     CARTAB        BEGSR
090000091027     c*---
090100091027     c* tipi legami "internet"
090200091027     c*---
090300091027     c                   z-add     *zeros        jtle
090400091027     c                   movel     'TLE'         tbecod
090500091027     c     tbecod        setll     tntbe01l
090600091027     c     tbecod        reade     tntbe01l
090700091027     c                   dow       not %eof(tntbe01l)
090800091027     c                   if        tbeatb = *blanks
090900091027     c                   eval      dtle = tbeuni
091000091027     c* solo legami d tipo "internet"
091100091027     c                   if        §TLEGRP = 'W'
091200091027     c                   add       1             jtle
091300091027     c                   eval      sktle(jtle) = tbeke1
091400091027     c                   endif
091500091027     c                   endif
091600091027     c     tbecod        reade     tntbe01l
091700091027     c                   enddo
091800091027     C*
091900091027     C                   ENDSR
092000970526     C*------------------------------------------------------------------------*
092100970526     C* *INZSR - OPERAZIONI INIZIALI
092200970526     C*------------------------------------------------------------------------*
092300970526     C     *INZSR        BEGSR
092400970526     C*---
092500970526     C* RICEVIMENTO PARAMETRI
092600970526     C*---
092700970526     C     *ENTRY        PLIST
092800970612     C                   PARM                    KPJBA
092900050105     c                   Parm                    Tis730ds
093000050105
093100050105     c                   If        parms = 1
093200050105     c                   clear                   wricpgm           1
093300050105     c                   else
093400050105     c                   move      s30ric        wricpgm
093500050105     c                   endif
093600020924     C*
093700020924     C* REPERISCE I DATI DELL'UTENTE
093800020924     C                   EXSR      REPDATIUTE
093900970526     C*
094000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
094100970526     C     XMSGPA        PLIST
094200970526     C                   PARM                    DSMSMS
094300970526     C                   PARM                    DSMSPG
094400970526     C                   PARM                    DSMSKE
094500970526     C                   PARM                    DSMSER
094600970526     C*
094700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
094800970526     C                   CALL      'XKEY'
094900970526     C                   PARM                    DSKEY
095000970526     C*---
095100970526     C* VARIABILI NON RIFERITE AL DATA BASE
095200970526     C*---
095300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
095400000714     C     *LIKE         DEFINE    s20OP0        DEPOP0
095500000714     C     *LIKE         DEFINE    s20OP1        DEPOP1
095600000714     C     *LIKE         DEFINE    s20OP0        WOP0
095700000714     C     *LIKE         DEFINE    s20OP1        WOP1
095800970526     C*---
095900970526     C* VARIABILI DI CONTROLLO
096000970526     C*---
096100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
096200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
096300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
096400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
096500970526     C*
096600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
096700000714     C                   MOVEL     'TIS730R'     DSMSPG
096800000714     C                   MOVEL     'TIS730R'     MSGQUE
096900970526     C                   MOVEL     *BLANKS       MSGKEY
097000970526     C                   MOVEL     *BLANKS       DSMSMS
097100970526     C*---
097200970526     C* VARIABILI RIFERITE AL DATA BASE
097300970526     C*---
097400991104     C     *LIKE         DEFINE    vssisv        KCEisv
097500991105     C     *LIKE         DEFINE    vsssun        KCEsun
097600000317     C     *LIKE         DEFINE    vssksu        KCEksu
097700100930     C     *LIKE         DEFINE    abltip        KCEtip
097800970526     C*---
097900970526     C* CHIAVI DI LETTURA
098000970526     C*---
098100100930     C* LETTURA tiabl01L - COMPLETA
098200100930     C     KEYabl        KLIST
098300100930     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
098400100930     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
098500100930     C                   KFLD                    KCEtip                         *TIPO SERVIZIO
098600991104     C* LETTURA tivss01L - COMPLETA
098700991104     C     KEYvss        KLIST
098800001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
098900001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
099000001116     C* LETTURA tivss02L - COMPLETA
099100001116     C     KE2vss        KLIST
099200001116     C                   KFLD                    KCEksu
099300001116     C                   KFLD                    KCEisv
099400020228     C* LETTURA tikun06L
099500020228     C     KE6kun        KLIST
099600020228     C                   KFLD                    KUNcof                         *CODICE CLI FIGLIO
099700020228     C                   KFLD                    KUNtle                         *TIPO LEGAME
099800000714     C*
099900000714     C* IMPOSTO SUBITO ALCUNE VARIABILI
100000001116     C                   EVAL      VIDisv = 'TT'
100100000721     C                   EXSR      CALORA
100200050104     C***                EVAL      DATAI = AAAA+'0101'
100300050104     C                   MOVE(P)   DATCOR        DATAI
100400060726     C                   EVAL      DATAF = '20391231'
100500020307     C*
100600020307     C* IMPOSTO QUESTE DATE CALCOLATE COME DATE D DEFAULT A VIDEO
100700020307     C                   MOVE(p)   DATAI         VIDDAI
100800020307     C                   MOVE(p)   DATAF         VIDDAF
100900000714     C*
101000000714     C* DEPOSITA LE OPZIONI DI ENTRATA
101100000714     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
101200000714     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
101300970526     C*
101400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
101500970526     C                   EXSR      IMPFUN
101600000131     C*
101700970526     C                   ENDSR
101800970526     C*------------------------------------------------------------------------*
101900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
102000970526     C*------------------------------------------------------------------------*
102100970526     C     FUNGES        BEGSR
102200970526     C*
102300970526     C* PULISCO CAMPI DI LAVORO
102400970526     C                   Z-ADD     0             W                 5 0
102500970526     C*
102600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
102700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
102800970526     C     1             DO        24            I
102900970526     C                   MOVEL     FUD(I)        FUDRIG
103000970526     C*
103100970526     C* PULIZIA FUNZIONI PRECEDENTI
103200970526     C                   MOVEL     *BLANKS       FUV(I)
103300970526     C*
103400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
103500970526     C* (SALTA F24)
103600970526     C     FUDATT        IFEQ      'S'
103700970526     C     FUDFUN        ANDNE     '24'
103800970526     C                   ADD       1             W
103900970526     C                   MOVEL     FUDDES        FUV(W)
104000970526     C                   END
104100970526     C                   END                                                    DO WHILE
104200970526     C*
104300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
104400970526     C* ALTRIMENTI DISATTIVO F24
104500970526     C     1             DO        24            I
104600970526     C                   MOVEL     FUD(I)        FUDRIG
104700970526     C*
104800970526     C     FUDFUN        IFEQ      '24'
104900970526     C     W             IFGT      6
105000970526     C                   MOVEL     'S'           FUDATT
105100970526     C                   ELSE
105200970526     C                   MOVEL     *BLANKS       FUDATT
105300970526     C                   END                                                    W GT 8
105400970526     C                   MOVEL     FUDRIG        FUD(I)
105500970526     C                   END
105600970526     C                   END                                                    DO WHILE
105700970526     C*
105800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
105900970526     C     W             DIV       6             I
106000970526     C                   MVR                     Z
106100970526     C*
106200970526     C     Z             IFNE      0
106300970526     C     I             ADD       1             Z
106400970526     C                   ELSE
106500970526     C                   Z-ADD     I             Z                 5 0
106600970526     C                   END
106700970526     C*
106800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
106900970526     C                   ADD       1             FUNGRU
107000970526     C     FUNGRU        IFGT      Z
107100970526     C     FUNGRU        ORLT      1
107200970526     C                   Z-ADD     1             FUNGRU
107300970526     C                   END
107400970526     C*
107500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
107600970526     C     FUNGRU        IFEQ      1
107700970526     C                   MOVEL     FUV(1)        FUN001
107800970526     C                   MOVEL     FUV(2)        FUN002
107900970526     C                   MOVEL     FUV(3)        FUN003
108000970526     C                   MOVEL     FUV(4)        FUN004
108100970526     C                   MOVEL     FUV(5)        FUN005
108200970526     C                   MOVEL     FUV(6)        FUN006
108300970526     C                   END
108400970526     C*
108500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
108600970526     C     FUNGRU        IFEQ      2
108700970526     C                   MOVEL     FUV(7)        FUN001
108800970526     C                   MOVEL     FUV(8)        FUN002
108900970526     C                   MOVEL     FUV(9)        FUN003
109000970526     C                   MOVEL     FUV(10)       FUN004
109100970526     C                   MOVEL     FUV(11)       FUN005
109200970526     C                   MOVEL     FUV(12)       FUN006
109300970526     C                   END
109400970526     C*
109500970526     C* TERZO GRUPPO DI 6 FUNZIONI
109600970526     C     FUNGRU        IFEQ      3
109700970526     C                   MOVEL     FUV(13)       FUN001
109800970526     C                   MOVEL     FUV(14)       FUN002
109900970526     C                   MOVEL     FUV(15)       FUN003
110000970526     C                   MOVEL     FUV(16)       FUN004
110100970526     C                   MOVEL     FUV(17)       FUN005
110200970526     C                   MOVEL     FUV(18)       FUN006
110300970526     C                   END
110400970526     C*
110500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
110600970526     C     FUNGRU        IFEQ      4
110700970526     C                   MOVEL     FUV(19)       FUN001
110800970526     C                   MOVEL     FUV(20)       FUN002
110900970526     C                   MOVEL     FUV(21)       FUN003
111000970526     C                   MOVEL     FUV(22)       FUN004
111100970526     C                   MOVEL     *BLANKS       FUN005
111200970526     C                   MOVEL     *BLANKS       FUN006
111300970526     C                   END
111400970526     C*
111500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
111600970526     C                   MOVEL     *BLANKS       FUN007
111700970526     C                   MOVEL     *BLANKS       FUN008
111800970526     C*
111900970526     C     1             DO        24            I
112000970526     C                   MOVEL     FUD(I)        FUDRIG
112100970526     C     FUDFUN        IFEQ      '24'
112200970526     C     FUDATT        ANDEQ     'S'
112300970526     C                   MOVEL     FUDDES        FUN008
112400970526     C                   END
112500970526     C                   END                                                    DO
112600970526     C*
112700970526     C                   ENDSR
112800970526     C*------------------------------------------------------------------------*
112900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
113000970526     C*------------------------------------------------------------------------*
113100970526     C     FUNCON        BEGSR
113200970526     C*
113300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
113400970526     C                   EXSR      FUNVAR
113500970526     C*
113600970526     C* PULISCO CAMPI DI LAVORO
113700970526     C                   Z-ADD     0             I                 5 0
113800970526     C                   Z-ADD     0             W                 5 0
113900970526     C*
114000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
114100970526     C     FUNKEY        IFNE      *BLANKS
114200970526     C     1             DO        24            I
114300970526     C                   MOVEL     FUD(I)        FUDRIG
114400970526     C*
114500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
114600970526     C     FUNKEY        IFEQ      FUDFUN
114700970526     C     FUDATT        ANDEQ     'S'
114800970526     C                   Z-ADD     I             W
114900970526     C                   END
115000970526     C                   END                                                    DO WHILE
115100970526     C*
115200970526     C* FUNZIONE NON ATTIVA (ERRORE)
115300970526     C     W             IFEQ      0
115400970526     C*
115500970526     C* EMISSIONE MESSAGGIO DI ERRORE
115600970526     C                   MOVEL     MSG(1)        DSMSMS
115700970526     C*
115800970526     C                   END                                                    W=0
115900970526     C                   END                                                    FUNKEY NE *BLANKS
116000970526     C*
116100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
116200970526     C     W             IFNE      0
116300970526     C     FUNKEY        OREQ      *BLANKS
116400970526     C     FUNKEY        IFEQ      *BLANKS
116500970526     C                   EXSR      ENTGES
116600970526     C                   END
116700970526     C     FUNKEY        IFEQ      '03'
116800970526     C                   EXSR      F03GES
116900970526     C                   END
117000001116     C     FUNKEY        IFEQ      '05'
117100001116     C                   EXSR      F05GES
117200001116     C                   END
117300970526     C     FUNKEY        IFEQ      '06'
117400970526     C                   EXSR      F06GES
117500970526     C                   END
117600001116     C     FUNKEY        IFEQ      '11'
117700001116     C                   EXSR      F11GES
117800001116     C                   END
117900970526     C     FUNKEY        IFEQ      '12'
118000970526     C                   EXSR      F12GES
118100970526     C                   END
118200970526     C     FUNKEY        IFEQ      '24'
118300970526     C                   EXSR      F24GES
118400970526     C                   END
118500970526     C                   END
118600970526     C*
118700970526     C                   ENDSR
118800970526     C*------------------------------------------------------------------------*
118900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
119000970526     C*------------------------------------------------------------------------*
119100970526     C     FUNVAR        BEGSR
119200970526     C*
119300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
119400970526     C*
119500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
119600970526     C     KEY           IFEQ      ENTER
119700970526     C                   MOVEL     *BLANKS       FUNKEY
119800970526     C                   END
119900970526     C     KEY           IFEQ      F1
120000970526     C                   MOVEL     '01'          FUNKEY
120100970526     C                   END
120200970526     C     KEY           IFEQ      F2
120300970526     C                   MOVEL     '02'          FUNKEY
120400970526     C                   END
120500970526     C     KEY           IFEQ      F3
120600970526     C                   MOVEL     '03'          FUNKEY
120700970526     C                   END
120800970526     C     KEY           IFEQ      F4
120900970526     C                   MOVEL     '04'          FUNKEY
121000970526     C                   END
121100970526     C     KEY           IFEQ      F5
121200970526     C                   MOVEL     '05'          FUNKEY
121300970526     C                   END
121400970526     C     KEY           IFEQ      F6
121500970526     C                   MOVEL     '06'          FUNKEY
121600970526     C                   END
121700970526     C     KEY           IFEQ      F7
121800970526     C                   MOVEL     '07'          FUNKEY
121900970526     C                   END
122000970526     C     KEY           IFEQ      F8
122100970526     C                   MOVEL     '08'          FUNKEY
122200970526     C                   END
122300970526     C     KEY           IFEQ      F9
122400970526     C                   MOVEL     '09'          FUNKEY
122500970526     C                   END
122600970526     C     KEY           IFEQ      F10
122700970526     C                   MOVEL     '10'          FUNKEY
122800970526     C                   END
122900970526     C     KEY           IFEQ      F11
123000970526     C                   MOVEL     '11'          FUNKEY
123100970526     C                   END
123200970526     C     KEY           IFEQ      F12
123300970526     C                   MOVEL     '12'          FUNKEY
123400970526     C                   END
123500970526     C     KEY           IFEQ      F13
123600970526     C                   MOVEL     '13'          FUNKEY
123700970526     C                   END
123800970526     C     KEY           IFEQ      F14
123900970526     C                   MOVEL     '14'          FUNKEY
124000970526     C                   END
124100970526     C     KEY           IFEQ      F15
124200970526     C                   MOVEL     '15'          FUNKEY
124300970526     C                   END
124400970526     C     KEY           IFEQ      F16
124500970526     C                   MOVEL     '16'          FUNKEY
124600970526     C                   END
124700970526     C     KEY           IFEQ      F17
124800970526     C                   MOVEL     '17'          FUNKEY
124900970526     C                   END
125000970526     C     KEY           IFEQ      F18
125100970526     C                   MOVEL     '18'          FUNKEY
125200970526     C                   END
125300970526     C     KEY           IFEQ      F19
125400970526     C                   MOVEL     '19'          FUNKEY
125500970526     C                   END
125600970526     C     KEY           IFEQ      F20
125700970526     C                   MOVEL     '20'          FUNKEY
125800970526     C                   END
125900970526     C     KEY           IFEQ      F21
126000970526     C                   MOVEL     '21'          FUNKEY
126100970526     C                   END
126200970526     C     KEY           IFEQ      F22
126300970526     C                   MOVEL     '22'          FUNKEY
126400970526     C                   END
126500970526     C     KEY           IFEQ      F23
126600970526     C                   MOVEL     '23'          FUNKEY
126700970526     C                   END
126800970526     C     KEY           IFEQ      F24
126900970526     C                   MOVEL     '24'          FUNKEY
127000970526     C                   END
127100970526     C     KEY           IFEQ      ROLLUP
127200970526     C                   MOVEL     'RU'          FUNKEY
127300970526     C                   END
127400970526     C     KEY           IFEQ      HELP
127500970526     C                   MOVEL     'HP'          FUNKEY
127600970526     C                   END
127700970526     C                   ENDSR
127800970526     C*------------------------------------------------------------------------*
127900970526     C* NOTE COSTRUZIONE SCHIERE
128000970526     C*------------------------------------------------------------------------*
128100970526     C*
128200970526     C* -MSG-MESSAGGI
128300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
128400970526     C* (NON TOCCARE MSG,1 E MSG,2)
128500970526     C*
128600970526     C* -CMD-COMANDI
128700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
128800970526     C*
128900970526     C* -OPD-OPZIONI
129000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
129100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
129200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
129300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
129400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
129500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
129600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
129700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
129800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
129900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
130000970526     C*
130100970526     C* -FUD-FUNZIONI
130200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
130300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
130400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
130500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
130600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
130700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
130800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
130900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
131000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
131100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
131200970526     O*------------------------------------------------------------------------*
131300970526** DEC - DECODIFICHE
131400001116  ISCRIZIONE                                                      1
131500970526** MSG - MESSAGGI
131600000714TIS730R- Codice Cliente inesistente                                            1
131700000714TIS725R- Inserimento effettuato                                                2
131800001220TIS730R- Ciente già abil. per stesso servizio con altro SGIUSR                 3
131900000714TIS730R- Campo obbligatorio                                                    4
132000000719TIS730R- Possibile errore remoto o di comunicazione                            5
132100000714TIS725R- Richiesta funzione per record inesistente                             6
132200000714TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
132300000728TIS730R- Cliente già iscritto in STRATEGI con codice:                          8
132400000717TIS730R- Iscritto ed abilitato Utente Strategi #                               9
132500001116TIS730R- Tipo servizio errato o inesistente                                    10
132600010605TIS730R- Nel periodo esistono già abilitazioni per il cliente al file TO.      11
132700010605TIS730R- Abilitazione per il cliente al file TO già esistente.                 12
132800020228TIS730R- Cliente già presente come figlio con legame WW nel periodo            13
132900100930TIS730R- Richiesta funzione per record inesistente                             14
133000100930TIS730R- Record momentaneamente allocato: riprovare più tardi                  15
133100100930TIS730R- Richiesta invio nuova password effettuato.                            16
133200100930TIS730R-                                                                       17
133300100930TIS730R-                                                                       18
133400100930TIS730R-                                                                       19
133500100930TIS730R-                                                                       20
133600970526** FUD - FUNZIONI
133700970526                   -        (S/N)=N                        funz.01¬
133800970526                   -        (S/N)=N                        funz.02¬
133900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
134000970526                   -        (S/N)=N                        funz.04¬
134100001116F05=Refresh        -        (S/N)=S                        funz.05¬
134200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
134300970527                   -        (S/N)=N                        funz.07¬
134400970527                   -        (S/N)=N                        funz.08¬
134500970527                   -        (S/N)=N                        funz.09¬
134600970527                   -        (S/N)=N                        funz.10¬
134700050105F11=Abilit. servizi-        (S/N)=S                        funz.11¬
134800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
134900970526                   -        (S/N)=N                        funz.13¬
135000970526                   -        (S/N)=N                        funz.14¬
135100970526                   -        (S/N)=N                        funz.15¬
135200970526                   -        (S/N)=N                        funz.16¬
135300970526                   -        (S/N)=N                        funz.17¬
135400970526                   -        (S/N)=N                        funz.18¬
135500001116                   -        (S/N)=N                        funz.19¬
135600970526                   -        (S/N)=N                        funz.20¬
135700970526                   -        (S/N)=N                        funz.21¬
135800970526                   -        (S/N)=N                        funz.22¬
135900970526                   -        (S/N)=N                        funz.23¬
136000050105F24=Altri tasti    -        (S/N)=N                        funz.24¬
