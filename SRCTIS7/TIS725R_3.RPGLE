000100970526     H*------------------------------------------------------------------------*
000200120104     H* Gestione codici clienti strategi
000300970526     H*------------------------------------------------------------------------*
000400090714     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991104     FtiS725d   cf   e             WORKSTN INFDS(DEVDS1)
001000991104     F                                     SFILE(S725DMS:MSGULT)
001100970527     F*------------
001200991104     Ftivss01L  UF A E           K DISK
001300000317     Ftivss02L  IF   E           K DISK
001400000807     F                                     RENAME(tivss000:tivss002)
001500000131     Ftikun01L  IF   E           K DISK
001600000215     Ftikun03L  IF   E           K DISK
001700000807     F                                     RENAME(tikun000:tikun003)
001800020211     Ftikun06L  IF   E           K DISK
001900020211     F                                     RENAME(tikun000:tikun006)
002000101014     Ftivir03L  IF A E           K DISK
002100120327     F***cnaco00f  IF   E           K DISK
002200000719     Fsgius     IF   E           K disk    usropn
002300000728     Ftianv01l  IF   E           K DISK
002400101014     Ftntbe01l  IF   E           K DISK
002500970526     D*------------------------------------------------------------------------*
002600970526     D* SCHIERE
002700970526     D*------------------------------------------------------------------------*
002800970526     D*------------
002900970526     D* DECODIFICHE
003000970526     D*------------
003100991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
003200000215     D*------------
003300000215     D* DESCRIZIONI
003400000215     D*------------
003500000215     D DES             S             66    DIM(5) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* MESSAGGI
003800970526     D*------------
003900120327     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* DESCRIZIONI TASTI FUNZIONALI
004200970526     D*------------
004300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970526     D FUV             S             19    DIM(24)
004800970526     D*------------------------------------------------------------------------*
004900970526     D* INPUT
005000970526     D*------------------------------------------------------------------------*
005100970526     D*------------
005200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005300970526     D*------------
005400970526     D FUDDS           DS
005500970526     D  FUDDE1                 1      1
005600970526     D  FUDFUN                 2      3
005700970526     D  FUDDE2                 4     19
005800970526     D  FUDFI0                20     34
005900970526     D  FUDATT                35     35
006000970526     D  FUDFI1                36     80
006100970526     D  FUDDES                 1     19
006200970526     D  FUDRIG                 1     80
006300970526     D*------------
006400970526     D* DS GESTIONE TASTI FUNZIONALI
006500970526     D*------------
006600970526     D DSKEY         E DS
006700970526     D DEVDS1          DS
006800970526     D  KEY                  369    369
006900970526     D*------------
007000970526     D* DS "XMSG" - GESTIONE MESSAGGI
007100970526     D*------------
007200970526     D DSMSG         E DS
007300020212     D*------------
007400020212     D* DS X LETTURA NUOVA TABELLA ISV
007500020212     D*------------
007600091027     D dsisv         E DS                  extname(disv)
007700091027     D dstle         E DS                  extname(dtle)
007800000317     D*------------
007900000807     D* REPERIMENTO CLIENTI DA CLIENTE UNIFICANTE
008000000317     D*------------
008100000807     D tibs10ds      E DS
008200970528     D*------------------
008300970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
008400970528     D*------------------
008500000317     D WLBDA8          DS                  INZ
008600970528     D  G08DAT                 1      8  0
008700970528     D  G08INV                 9     16  0
008800970528     D  G08ERR                17     17
008900970528     D  G08TGI                18     22  0
009000970526     D*------------
009100970526     D* DS DI PROCEDURA
009200970526     D*------------
009300991014     D TIBS02DS      E DS
009400991104     D tis720        E DS                  EXTNAME(tis720ds)
009500090804     D dvssdta       E DS
009600100217     D tivssds       E DS                  extname(tivss00f)
009700100914     D tis725ds21    E DS                  prefix(S725_)
009800970526     D*------------
009900970526     D* ARCHITETTURA
010000970526     D*------------
010100970526     D KPJBA         E DS
010200100406     D*------------
010300100406     D* DS DEFINIZIONE TABELLA '3EW'
010400100406     D*------------
010500100406     D D3EW          E DS                  qualified
010600000807     D*------------
010700000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
010800000807     D ksc             s              7  0 dim(30)
010900000807     D ksa             s              4    dim(30)
011000020212     D isvkun          s              2    dim(100)
011100050118     D isvtt           s              2    dim(100)
011200100913     D isvftp          s              2    dim(100)
011300100217     D isvKEYS         s              2    dim(100)
011400100217     D isvCTRL         s              4    dim(100)
011500101006     D isvPGMAFT       s              3    dim(100)
011600000807     D*------------------
011700000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
011800000807     D*------------------
011900000807     DDSXC           e DS                  extname(XCLIRDS)
012000000807     D*---
012100000807     D                 DS
012200000807     D dsksn                   1      4p 0
012300000807     D dsksa                   1      4
012400000807     DBS69DS         e ds                  extname(TIBS69DS)
012500000807     DACODS          e ds                  extname(cnaco00f)
012600000807     DINDDS          e ds                  extname(cnind00f)
012700000807     DCLPDS          e ds                  extname(cnclp00f)
012800000807     DCLSDS          e ds                  extname(fncls00f)
012900000807     D*---
013000000807     D* VARIABILI NON RIFERITE AL DATABASE
013100000807     D*---
013200000807     D $esikun         S              1                                         *Esistenza KUN
013300000807     D chk             S              1    inz('1')
013400000807     D giro            S              1  0 inz(*zeros)
013500000807     D tipprf          s              3    inz(*blanks)
013600040422     D cmd             s            500
013700040422     D api             s              1    inz('''')
013800090611     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
013900091027     D sktle           S              2    DIM(20)
014000091027     D jtle            S              5  0                                      *indici
014100100914     D OPERAZ          S              3A   inz
014200100914     D savKPJBU        S                   like(KPJBU)
014300100914     D ds_param        S           1024    inz
014400100913     D*
014500100913     D                 ds
014600100913     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
014700100913     D                                     456789abcdefghijklmnopqrstuvwxyz')
014800100913     D  valAry                        1A   DIM(63)
014900100913     D                                     OVERLAY(val)
015000000807     D*------------
015100000807     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
015200000807     D*------------
015300000807     D                 DS
015400000807     D  FILCLI                 5      7  0
015500000807     D  STRATECLI              1     11
015600020924     D*-------------------
015700020924     D* DS REPERIMENTO DATI UTENTE
015800020924     D*-------------------
015900020924     D TIBS34DS      E DS                                                       *Profili utente
016000020924     D DDATIUTE      E DS                                                       *Dati utente
016100020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016200111216     D*------------------
016300111216     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
016400111216     D*------------------
016500111216     D currSysNeta     s              8A   inz('*NULL')
016600111216     D AS400_ok        s              1A
016700111216     D  PiOutSysNeta   S              8A
016800090610     D*-------------------
016900090610     D* DEFINIZIONI PROCEDURE ESTERNE
017000090610     D*-------------------
017100090610     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
017200090610     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
017300101022     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
017400101022     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
017500100406     D/COPY GAITRASRC/SRCPROTOPR,UB§3EWUPD
017600110419     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
017700110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
017800111216     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
017900970526     C*------------------------------------------------------------------------*
018000970526     C* MAIN LINES
018100970526     C*------------------------------------------------------------------------*
018200970526     C*
018300970526     C* NON CI SONO ERRORI IN ENTRATA
018400000915IF  1C*    ERRL00        IFEQ      '0'
018500970526     C*
018600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
018700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
018800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
018900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
019000970526     C*
019100970526     C* EMETTE FORMATO VIDEO
019200970526     C                   EXSR      BUTFMR
019300970526     C*
019400970526     C* GESTIONE TASTI FUNZIONALI
019500970526     C                   EXSR      FUNCON
019600970526E   2C                   ENDDO
019700970526     C*
019800970526     C* CI SONO ERRORI IN ENTRATA
019900000915X   1C*                  ELSE
020000000915     C*                  MOVEL     '1'           s20err
020100000915     C*                  MOVEL     DSMSMS        s20msg
020200000915E   1C*                  ENDIF
020300001116     C*
020400970526     C* FINE PGM
020500001116     C                   EXSR      FINSR
020600001116     C*
020700100914     C                   MOVEL     tis720        KPJBU
020800101022     C*
020900101022     C* Chiudo la connessione
021000111216     C                   IF        AS400_ok = *on
021100101022     C                   exsr      clsCONN
021200111216     C                   ENDIF
021300101022     C*
021400970526     C                   SETON                                        LR
021500100913     C*------------------------------------------------------------------------*
021600100913     C* RESETIN - RESET INDIRCATORI
021700100913     C*------------------------------------------------------------------------*
021800100913     C     RESETIN       BEGSR
021900100913     C*
022000100913     C* REIMPOSTA INDICATORI DI ERRORE
022100100913     C                   SETOFF                                         5859
022200100913     C                   SETOFF                                       606162
022300100913     C                   SETOFF                                       636465
022400100913     C                   SETOFF                                       666768
022500100913     C                   SETOFF                                       697071
022600100913     C                   SETOFF                                       727374
022700100913     C                   SETOFF                                       757677
022800100913     C                   SETOFF                                       787980
022900100913     C*
023000100913     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     BUTFMR        BEGSR
023500970526     C*
023600970526     C* EMETTE LA TESTATA
023700991104     C                   WRITE     S725D01
023800970526     C*
023900970526     C* SPEDISCE EVENTUALE MESSAGGIO
024000970526     C                   EXSR      SNDMSG
024100970526     C*
024200970526     C* EMETTE LE FUNZIONI
024300991104     C                   WRITE     S725D02
024400970526     C*
024500970526     C* EMETTE IL VIDEO
024600991104     C                   EXFMT     S725D03
024700970526     C*
024800970526     C* REIMPOSTA FLAG DI ERRORE
024900970526     C                   MOVEL     '0'           ERRL00
025000100913     C*
025100100913     C* RESETTA INDICATORI DI CONTROLLO
025200100913     C                   EXSR      RESETIN
025300970526     C*
025400970526     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
025700970526     C*------------------------------------------------------------------------*
025800970526     C     SNDMSG        BEGSR
025900970526     C*
026000970526     C* PULISCE IL SFILE MESSAGGI
026100970526     C                   MOVEL     '1'           *IN40
026200991104     C                   WRITE     S725DMC
026300970526     C                   MOVEL     '0'           *IN40
026400970526     C*
026500970526     C* VISUALIZZA IL MESSAGGIO
026600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
026700970526     C                   CALL      'XMSG'        XMSGPA
026800970526IF  1C     DSMSER        IFEQ      *ZEROS
026900970526     C                   MOVEL     DSMSKE        MSGKEY
027000970526E   1C                   ENDIF
027100970526     C                   Z-ADD     1             MSGULT
027200991104     C                   WRITE     S725DMS
027300991104     C                   WRITE     S725DMC
027400970526     C*
027500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
027600970526     C                   MOVEL     *BLANKS       DSMSMS
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* ENTGES - GESTIONE ENTER -> INVIO
028100970526     C*------------------------------------------------------------------------*
028200970526     C     ENTGES        BEGSR
028300970526     C*
028400970526     C* CONTROLLA I CAMPI IMMESSI
028500970526     C                   EXSR      DECODI
028600970526     C*
028700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
028800970526IF  1C     DEPOP1        IFEQ      'O05'
028900970526     C                   MOVEL     '1'           FINL00
029000970526E   1C                   ENDIF
029100970526     C*
029200970526     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* F03GES - GESTIONE F03 -> FINE
029500970526     C*------------------------------------------------------------------------*
029600970526     C     F03GES        BEGSR
029700970526     C*
029800970526     C* IMPOSTA "PREMUTO F03"
029900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
030000991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
030100970526     C*
030200970526     C                   ENDSR
030300970526     C*------------------------------------------------------------------------*
030400970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
030500970526     C*------------------------------------------------------------------------*
030600970526     C     F06GES        BEGSR
030700970526     C*
030800970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
030900970526IF  1C     DEPOP1        IFEQ      'F06'
031000100914     C                   EVAL      OPERAZ = 'INS'
031100970526     C                   EXSR      INSERI
031200970526E   1C                   ENDIF
031300970526IF  1C     DEPOP1        IFEQ      'O02'
031400100914     C                   EVAL      OPERAZ = 'UPD'
031500970526     C                   EXSR      MODIFI
031600090610     C                   EXSR      CHKSGI
031700970526E   1C                   ENDIF
031800991014IF  1C     DEPOP1        IFEQ      'O03'
031900100914     C                   EVAL      OPERAZ = 'INS'
032000000719     C                   EXSR      COPIA
032100991014E   1C                   ENDIF
032200021024IF  1C     DEPOP1        IFEQ      'O04'
032300100914     C                   EVAL      OPERAZ = 'DEL'
032400021024     C                   EXSR      ANNULL
032500090610     C                   EXSR      CHKSGI
032600021024E   1C                   ENDIF
032700970526IF  1C     DEPOP1        IFEQ      'O07'
032800100914     C                   EVAL      OPERAZ = 'UPD'
032900970526     C                   EXSR      RIPRIS
033000090610     C                   EXSR      CHKSGI
033100970526E   1C                   ENDIF
033200101012     C*
033300101012     C                   ENDSR
033400101012     C*------------------------------------------------------------------------*
033500101012     C* EXEPOST - GESTIONE "POST AGGIORNAMNETO"
033600101012     C*------------------------------------------------------------------------*
033700101012     C     EXEPOST       BEGSR
033800101012     C*
033900100217     C*
034000110210     C* Se no errore => gestione eventuale *PGM POST-AGGIORNAMENTO (solo x "INSERIMENTI")
034100110214 xxx C***                if        ERRL00 <> '1' and OPERAZ = 'INS'
034200110214 xxx C                   if        ERRL00 <> '1'
034300101005     C                   seton                                        33
034400101005     C*
034500101006     C                   movel     *blanks       wPGMAFTER         3
034600100217     C                   z-add     1             jISV              3 0
034700100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
034800100217     C                   if        *in22 = *on
034900100217     C                   if        isvPGMAFT(jISV) <> *blanks
035000100217     C                   eval      wPGMAFTER = isvPGMAFT(jISV)
035100100914     C                   eval      savKPJBU = KPJBU
035200100914     C*
035300101004     C                   select
035400110210     C* - 002
035500110210     C                   when      wPGMAFTER = '002'
035600100914     C                   clear                   TIS725DS21
035700100914     C                   select
035800110210     C                   when      FUNKEY = '06'
035900101022 xxx C***                setoff                                       33
036000101005     C                   eval      s725_§21_FILE   = 'VSS'
036100100914     C                   when      FUNKEY = '18'
036200110210 xxx C***                setoff                                       33
036300100914     C                   eval      s725_§21_FILE   = 'ESY'
036400100914     C                   endsl
036500100914     C                   eval      s725_§21_OPERAZ = OPERAZ
036600100914     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
036700100914     C                   eval      s725_§21_DESCR  = VDDKSU
036800100914     C                   eval      s725_§21_PASSW  = VIDPWDFTP
036900100914     C                   eval      ds_param = TIS725DS21
037000101006     C* - 003
037100110210     C                   when      wPGMAFTER = '003'
037200101005     C                   clear                   TIS725DS21
037300101022 xxx C***                setoff                                       33
037400101005     C                   eval      s725_§21_FILE   = 'VSS'
037500101005     C                   eval      s725_§21_OPERAZ = OPERAZ
037600101005     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
037700101005     C                   eval      s725_§21_DESCR  = VDDKSU
037800101005     C                   eval      s725_§21_PASSW  = VIDPWDFTP
037900101005     C                   eval      ds_param = TIS725DS21
038000101004     C*
038100101004     C                   endsl
038200100914     C*
038300101005     C   33              call      'TIS725R1'
038400100914     C                   parm                    KPJBA
038500100217     C                   parm                    wPGMAFTER
038600100217     C                   parm                    tivssds
038700100914     C                   parm                    ds_param
038800100914     C*
038900101006     C                   if        wPGMAFTER = '002' OR
039000101006     C                             wPGMAFTER = '003'
039100100914     C                   eval      TIS725DS21 = ds_param
039200100914     C                   if        s725_§21_ERRFLG <> *blanks
039300100914     C                   eval      s20MSG = s725_§21_ERRMSG
039400100914     C                   eval      DSMSMS = s725_§21_ERRMSG
039500100914     C                   endif
039600100914     C                   if        s725_§21_ERRFLG <> *blanks
039700100914     C                   MOVEL     '1'           ERRL00
039800100914     C                   endif
039900100914     C                   endif
040000100914     C*
040100100914     C                   eval      KPJBU = savKPJBU
040200100217     C                   endif
040300100217     C                   endif
040400101005     C*
040500101005     C                   endif
040600970526     C*
040700970526     C                   ENDSR
040800970526     C*------------------------------------------------------------------------*
040900970526     C* F12GES - GESTIONE F12 -> RITORNO
041000970526     C*------------------------------------------------------------------------*
041100970526     C     F12GES        BEGSR
041200970526     C*
041300970526     C* IMPOSTA "PREMUTO F12"
041400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
041500991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
041600970526     C*
041700970526     C                   ENDSR
041800100914     C*------------------------------------------------------------------------*
041900100914     C* F18GES - GESTIONE F18 -> AGGIORNAMENTO EASYSPED
042000100914     C*------------------------------------------------------------------------*
042100100914     C     F18GES        BEGSR
042200100914     C*
042300100914     C                   EXSR      F06GES
042400110419     C*
042500110419     C* Forzo l'elaborazione lato server
042600110419     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
042700110419     C                                           '0ADMIN':'#U')
042800100914     C*
042900100914     C                   ENDSR
043000970526     C*------------------------------------------------------------------------*
043100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
043200970526     C*------------------------------------------------------------------------*
043300970526     C     F24GES        BEGSR
043400970526     C*
043500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
043600970526     C*
043700970526     C                   ENDSR
043800970526     C*------------------------------------------------------------------------*
043900010605     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
044000970526     C*------------------------------------------------------------------------*
044100970526     C     INSERI        BEGSR
044200970526     C*
044300970526     C* CONTROLLA I CAMPI IMMESSI
044400970526     C                   EXSR      DECODI
044500000731     C                   IF        chk = '1' and giro = 1
044600000719     C                   MOVEL     '1'           ERRL00
044700000719     C                   ENDIF
044800970526     C*
044900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
045000010605     C     ERRL00        IFEQ      '0'                                          *ANCORA NO ERRORI
045100000719     C                   MOVE      VIDsun        vsssun
045200010605     C                   MOVE      VIDisv        vssisv
045300991104     C                   EXSR      IMPvss                                       *ALTRI CAMPI
045400991104     C                   Z-ADD     *ZEROS        vssDTR                         *DATA TRASMISSIONE
045500991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
045600991104     C                   WRITE     tivss000
045700000719     C                   Z-ADD     *zeros        giro
045800100914     C                   MOVEL     MSG(2)        s20MSG                         *MESSAGGIO
045900100914     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
046000010605     C*
046100010605     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
046200010608     C                   IF        ERRL01 = '0'  and
046300010608     C                             VIDisv = 'TT'
046400010626 ORM C*                  EXSR      IMPvir
046500010626 ORM C*                  WRITE     tivir000
046600010605     C                   ENDIF
046700040422     C*
046800040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
046900040422     C                   EXSR      EXESGI
047000101012     C*
047100101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
047200101012     C                   EXSR      EXEPOST
047300970602     C*
047400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
047500970602     C                   EXSR      AZZVID
047600010605     C                   ENDIF
047700970526     C*
047800970526     C                   ENDSR
047900970526     C*------------------------------------------------------------------------*
048000970526     C* MODIFI - OPERAZIONI DI MODIFICA
048100970526     C*------------------------------------------------------------------------*
048200970526     C     MODIFI        BEGSR
048300970526     C*
048400970526     C* CONTROLLA I CAMPI IMMESSI
048500970526     C                   EXSR      DECODI
048600000719     C*--------------------
048700000719     C* codice strategi
048800000719     C*--------------------
048900000731     C                   IF        chk = '1' and giro = 1
049000000719     C                   MOVEL     '1'           ERRL00
049100000719     C                   ENDIF
049200970526     C*
049300970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
049400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049500991104     C                   EXSR      LETvss
049600970526     C*
049700970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
049800970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049900991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
050000110214     C*
050100110214     C                   SETOFF                                       41
050200110214     C                   IF        DDEVID > datcor or
050300110214     C                             DSCVID < datcor
050400110214     C                   SETON                                        41
050500110214     C                   ENDIF
050600110214     C*
050700110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
050800110214     C   41              EXSR      EXEPOST
050900110214     C*
051000991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
051100991104     C                   UPDATE    tivss000
051200101008     C*
051300101008     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
051400101008     C                   EXSR      EXESGI
051500101027     C                   EVAL      s20MSG = MSG(3)
051600101027     C                   EVAL      DSMSMS = MSG(3)
051700101022     C*
051800101022     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
051900101022     C                   IF        *IN80
052000111216     C                             and AS400_ok = *on
052100101022      /FREE
052200101022
052300101022         if UBBVSQL01_UpdUnqUtente(
052400101022                         pInConn
052500101022                       : %subst(VIDKSU:2:7)
052600101022                       : VIDPWDFTP) = 0;
052700101027         else;
052800101027              s20MSG = MSG(36);
052900101027              DSMSMS = MSG(36);
053000101022         endif;
053100101022
053200101022      /END-FREE
053300101022     C
053400101022     C                   ENDIF
053500101008     C*
053600000719     C                   Z-ADD     *zeros        giro
053700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053800110214     C*
053900110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
054000110214     C  N41              EXSR      EXEPOST
054100110214     C*
054200970526E   2C                   ENDIF
054300970526E   1C                   ENDIF
054400970526     C*
054500970526     C                   ENDSR
054600991014     C*------------------------------------------------------------------------*
054700000719     C* COPIA  - OPERAZIONI DI COPIA
054800991014     C*------------------------------------------------------------------------*
054900000719     C     COPIA         BEGSR
055000991014     C*
055100991014     C* CONTROLLA I CAMPI IMMESSI
055200991014     C                   EXSR      DECODI
055300000719     C*--------------------
055400000719     C* codice strategi
055500000719     C*--------------------
055600000731     C                   IF        chk = '1' and giro = 1
055700000719     C                   MOVEL     '1'           ERRL00
055800000719     C                   ENDIF
055900991014     C*
056000991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
056100991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
056200991105     C                   MOVEL     VIDsun        vsssun                         *CHIAVE
056300991104     C                   MOVEL     vidisv        vssisv
056400991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
056500991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
056600001116     C                   WRITE     tivss000
056700000719     C                   Z-ADD     *zeros        giro
056800991104     C                   MOVEL     MSG(3)        s20MSG
056900000719     C                   MOVEL     MSG(3)        DSMSMS
057000991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
057100010612     C*
057200010612     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
057300010612     C                   IF        ERRL01 = '0'  and
057400010612     C                             VIDisv = 'TT'
057500010626 ORM C*                  EXSR      IMPvir
057600010626 ORM C*                  WRITE     tivir000
057700010612     C                   ENDIF
057800040422     C*
057900040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
058000040422     C                   EXSR      EXESGI
058100101012     C*
058200101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
058300101012     C                   EXSR      EXEPOST
058400991014E   2C                   ENDIF
058500991014     C*
058600991014     C                   ENDSR
058700970526     C*------------------------------------------------------------------------*
058800970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
058900970526     C*------------------------------------------------------------------------*
059000991104     C*    ANNULL        BEGSR
059100970526     C*
059200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
059300991104     C*                  EXSR      LETvss
059400970527     C*
059500970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
059600991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
059700991104IF  2C*    vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
059800991104IF  3C*    vssDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
059900991104     C*                  MOVEL     'A'           vssvat                         *FLAG ANNULLAMENTO
060000991104     C*                  MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
060100991104     C*                  UPDATE    tivss000                                     *ANNULLA
060200991104X   3C*                  ELSE                                                   *MAI TRASMESSO
060300991104     C*                  DELETE    tivss000                                     *CANCELLA
060400991104E   3C*                  ENDIF
060500991104E   2C*                  ENDIF
060600991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
060700991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
060800991104E   1C*                  ENDIF
060900970526     C*
061000991104     C*                  ENDSR
061100021024     C*------------------------------------------------------------------------*
061200021024     C* ANNULL - OPERAZIONI DI FAR SCADERE
061300021024     C*------------------------------------------------------------------------*
061400021024     C     ANNULL        BEGSR
061500021024     C*
061600021024     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
061700021024     C                   EXSR      LETvss
061800021024     C*
061900021024     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
062000021024IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
062100021024IF  2C     vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
062200090611     C* Faccio scadere a oggi - 1 giorno
062300090611     C     *ISO          MOVE      datcor        wDataISO
062400090611     C     wDataISO      SUBDUR    1:*D          wDataISO
062500110214     C     *ISO          MOVE      wDataISO      DSCVID
062600090611     C     *ISO          MOVE      wDataISO      vssdsc
062700090611     C*
062800021024     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
062900110214     C*
063000110214     C                   SETOFF                                       41
063100110214     C                   IF        DDEVID > datcor or
063200110214     C                             DSCVID < datcor
063300110214     C                   SETON                                        41
063400110214     C                   ENDIF
063500110214     C*
063600110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
063700110214     C   41              EXSR      EXEPOST
063800110214     C*
063900021024     C                   UPDATE    tivss000                                     *ANNULLA
064000110214     C*
064100110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
064200110214     C  N41              EXSR      EXEPOST
064300110214     C*
064400021024E   2C                   ENDIF
064500021024     C                   MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
064600021024     C                   MOVEL     '1'           FINL00                         *FINE PGM
064700021024E   1C                   ENDIF
064800021024     C*
064900021024     C                   ENDSR
065000970526     C*------------------------------------------------------------------------*
065100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
065200970526     C*------------------------------------------------------------------------*
065300970526     C     RIPRIS        BEGSR
065400970526     C*
065500970526     C* CONTROLLA I CAMPI IMMESSI
065600970526     C                   EXSR      DECODI
065700000719     C*--------------------
065800000719     C* codice strategi
065900000719     C*--------------------
066000000731     C                   IF        chk = '1' and giro = 1
066100000719     C                   MOVEL     '1'           ERRL00
066200000719     C                   ENDIF
066300970526     C*
066400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
066500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
066600991104     C                   EXSR      LETvss
066700970526     C*
066800970526     C* IMPOSTA IL RECORD DA SCRIVERE
066900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
067000991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
067100991104     C                   UPDATE    tivss000                                     *ANNULLA
067200000719     C                   Z-ADD     *zeros        giro
067300970526E   2C                   ENDIF
067400991104     C                   MOVEL     MSG(5)        s20MSG                         *MESSAGGIO
067500000719     C                   MOVEL     MSG(5)        DSMSMS                         *MESSAGGIO
067600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
067700101012     C*
067800101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
067900101012     C                   EXSR      EXEPOST
068000970526E   1C                   ENDIF
068100970526     C*
068200970526     C                   ENDSR
068300970526     C*------------------------------------------------------------------------*
068400991104     C* LETvss - LEGGE IL RECORD PERL'AGGIORNAMENTO
068500970526     C*------------------------------------------------------------------------*
068600991104     C     LETvss        BEGSR
068700970526     C*
068800100322     C                   MOVE      vidsun        KCEsun
068900100924     C***                MOVE      vidisv        KCEisv
069000100924     C                   MOVE      vssisv        KCEisv
069100991104     C     KEYvss        CHAIN     tivss01L                           9998
069200970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
069300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
069400991104     C                   MOVEL     MSG(7)        s20MSG                         *MESSAGGIO
069500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
069600970526E   1C                   ENDIF
069700970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
069800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
069900991104     C                   MOVEL     MSG(6)        s20msg                         *MESSAGGIO
070000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
070100970526E   1C                   ENDIF
070200970526     C*
070300970526     C                   ENDSR
070400970526     C*------------------------------------------------------------------------*
070500970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
070600970526     C*------------------------------------------------------------------------*
070700970526     C     DECODI        BEGSR
070800000719     C*
070900000719     C                   MOVEL     '0'           ERRL00
071000000131     C*
071100991104     C*--------------------
071200991104     C* tipo servizio
071300991104     C*--------------------
071400991104IF  1C     ERRL00        IFEQ      '0'
071500991104IF  2C     VIDisv        IFne      *blank
071600020214     C                   EXSR      cntisv
071700991104X   2C                   ELSE
071800991104     C                   MOVEL     '1'           ERRL00
071900991104     C                   SETON                                        62
072000091117     C                   MOVEL     MSG(9)        DSMSMS
072100991104E   2C                   ENDIF
072200991104E   1C                   ENDIF
072300991008     C*--------------------
072400991008     C* tipo legame
072500991008     C*--------------------
072600991008IF  1C     ERRL00        IFEQ      '0'
072700020214     C*
072800100913     C* Verifico che il servizio del record corrente sia soggetto a controllo unificazioni
072900020214     C     VIDisv        lookup    isvkun                                 22
073000020214     C                   if        *in22 = *on
073100020214IF  2C     VIDtle        IFne      *blanks
073200091117     C     VIDtle        lookup    sktle                                  22
073300091117     C                   if        *in22 = *off
073400091117     C                   movel     '1'           errl00
073500091117     C                   seton                                        66
073600091117     C                   MOVEL     MSG(12)       DSMSMS
073700091117     C                   else
073800091117     C                   exsr      cnttle
073900091117     C                   endif
074000991008E   2C                   ENDIF
074100020214     C                   else
074200020214IF  2C     VIDtle        IFne      *blanks
074300020214     C                   MOVEL     '1'           ERRL00
074400020214     C                   SETON                                        66
074500020214     C                   MOVEL     MSG(28)       DSMSMS
074600020214E   2C                   ENDIF
074700020214     C                   endif
074800991008E   1C                   ENDIF
074900100913     C*
075000100913     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
075100100913     C     ERRL00        IFEQ      '0'
075200100913     C     VIDisv        lookup    isvftp                                 80
075300101022 xxx C***                setoff                                       80
075400100913     C                   ENDIF
075500101022     C*
075600970528     C*--------------------
075700991008     C* codice padre
075800970528     C*--------------------
075900991105     C     *LIKE         DEFINE    vssksu        ksuVID
076000970528IF  1C     ERRL00        IFEQ      '0'
076100991105     C                   MOVEL     *BLANKS       VDDksu
076200991105IF  2C     VIDksu        IFEQ      *blanks                                      *STANDARD
076300020214IF  2C     VIDksu        OREQ      *zeros                                       *STANDARD
076400000705     C                   MOVEL     '1'           ERRL00
076500000705     C                   SETON                                        61
076600000705     C                   MOVEL     MSG(19)       DSMSMS
076700991105     C                   MOVEL     *ZEROS        ksuVID
076800970618X   2C                   ELSE
076900991105     C                   MOVEL     VIDksu        Wksu
077000991105     C                   MOVEL     *blank        Wdksu
077100991105     C                   EXSR      CHKksu
077200021114     C                   IF        flgKSU = 'S'
077300991105     C                   MOVEL     Wksu          VIDksu
077400991105     C                   MOVEL     WDksu         VDDksu
077500000728     C                   MOVEL     VIDksu        ksuVID
077600021114     C                   ENDIF
077700970618IF  3C     ERRL00        IFEQ      '1'
077800000728     C                   SETON                                        61
077900000728     C                   MOVEL     MSG(13)       DSMSMS
078000970618E   3C                   ENDIF
078100970618E   2C                   ENDIF
078200970528E   1C                   ENDIF
078300991015     C*--------------------
078400991015     C* DATA DECORRENZA
078500991015     C*--------------------
078600991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
078700991015IF  1C     ERRL00        IFEQ      '0'
078800991015     C                   Z-ADD     VIDDDE        wda8
078900991015     C                   Z-ADD     *ZEROS        da8w
079000991015     C                   EXSR      CHKDA8
079100991015     C                   Z-ADD     WDA8          viddde
079200991015     C                   Z-ADD     DA8W          ddevid
079300991015IF  2C     ERRL00        IFEQ      '1'
079400991015     C                   SETON                                        63
079500991015     C                   MOVEL     MSG(14)       dsmsms
079600991015E   2C                   ENDIF
079700991015E   1C                   ENDIF
079800991014     C* inserimento codice azienda (attualmente non abilitato)
079900991011IF  1C     ERRL00        IFEQ      '0'
080000991104IF  2C     VIDazp        IFne      *blank
080100991104     C                   SETON                                        60
080200991011     C                   MOVEL     MSG(19)       dsmsms
080300991011     C                   MOVEL     '1'           errl00
080400991011E   2C                   ENDIF
080500991011E   1C                   ENDIF
080600991011     C* INSERIMENTO -> RECORD INESISTENTE
080700991011IF  1C     ERRL00        IFEQ      '0'
080800991011IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
080900001116IF  2C     DEPOP1        OREQ      'O03'                                        *INSERIMENTO
081000991104     C                   MOVE      VIDisv        kceisv
081100001116     C                   MOVE      vidsun        kcesun
081200991104     C     KEYvss        CHAIN(N)  tivss01L                           99
081300970528IF  3C     *IN99         IFEQ      *OFF
081400970528     C                   MOVEL     '1'           ERRL00
081500970528     C                   MOVEL     MSG(8)        DSMSMS
081600970528     C                   SETON                                        606162
081700970528     C                   SETON                                        63
081800970528E   3C                   ENDIF
081900970528E   2C                   ENDIF
082000000317E   1C                   ENDIF
082100000317     C*---
082200000317     C* INSERIMENTO -> record inesistente come figlio di un cliente unificante per il servizio
082300010522     C*                record inesistente come padre (se stesso) con altro utente strategi
082400000317     C*---
082500000317     C* Cerca il padre se c'è
082600001116     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
082700001116     C     ERRL00        IFEQ      '0'
082800001116     C                   CLEAR                   tibs10ds
082900001116     C                   EVAL      d10drf = datcor                              *data riferimento
083000010522     C                   EVAL      d10tle = vidtle                              *legame INTERNET
083100001116     C                   EVAL      d10paf = 'P'                                 *se esiste record
083200001116     C                   MOVE      ksuvid        n8                8 0
083300001116     C                   Z-ADD     n8            d10cod                         *cliente figlio
083400001116     C                   CALL      'TIBS10R'                                    *cerca figli
083500001116     C                   PARM                    tibs10ds
083600001116     C                   IF        d10err = ' '                                 *esiste padre
083700001116     C                   MOVE      d10cop        kceksu
083800001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
083900000317     C     KE2vss        SETLL     tivss02L
084000000317     C     KE2vss        READE     tivss02L                               99
084100001116     C     *IN99         DOWEQ     *OFF
084200001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
084300001116     C     ddevid        ANDLE     vssdsc
084400001116     C                   MOVEL     MSG(10)       DSMSMS
084500001116     C                   LEAVE
084600001116     C                   ENDIF
084700001116     C     KE2vss        READE     tivss02L                               99
084800001116     C                   ENDDO
084900001116     C                   ELSE                                                   *NON esiste padre
085000000317     C                   MOVE      ksuvid        kceksu
085100001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
085200000317     C     KE2vss        SETLL     tivss02L
085300000317     C     KE2vss        READE     tivss02L                               99
085400001116     C     *IN99         DOWEQ     *OFF
085500001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
085600001116     C     ddevid        ANDLE     vssdsc
085700001116     C                   MOVEL     MSG(11)       DSMSMS
085800001116     C                   LEAVE
085900001116     C                   ENDIF
086000000317     C     KE2vss        READE     tivss02L                               99
086100001116e   4C                   ENDDO
086200001116e   3C                   ENDIF
086300001116e   2C                   ENDIF
086400001116e   1C                   ENDIF
086500970528     C*--------------------
086600970528     C* DATA SCADENZA
086700970528     C*--------------------
086800970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
086900970528IF  1C     ERRL00        IFEQ      '0'
087000970528     C                   Z-ADD     VIDDSC        WDA8
087100970528     C                   Z-ADD     *ZEROS        DA8W
087200970528     C                   EXSR      CHKDA8
087300970528     C                   Z-ADD     WDA8          VIDDSC
087400970528     C                   Z-ADD     DA8W          DSCVID
087500970528IF  2C     ERRL00        IFEQ      '1'
087600970528     C                   SETON                                        64
087700970528     C                   MOVEL     MSG(14)       DSMSMS
087800970528E   2C                   ENDIF
087900970528E   1C                   ENDIF
088000970528     C* SCADENZA > DECORRENZA
088100970528IF  1C     ERRL00        IFEQ      '0'
088200970528IF  2C     DSCVID        IFLT      DDEVID
088300970619     C                   SETON                                        64
088400970528     C                   MOVEL     MSG(15)       DSMSMS
088500970528     C                   MOVEL     '1'           ERRL00
088600970528E   2C                   ENDIF
088700970528E   1C                   ENDIF
088800970528     C* SCADENZA >= DATA CORRENTE
088900010220IF  1C*    ERRL00        IFEQ      '0'
089000010220IF  2C*    DEPOP1        IFEQ      'O02'
089100010220     C*    DEPOP1        OREQ      'O07'
089200010220     C*    DEPOP1        OREQ      'F06'
089300010220IF  3C*    DSCVID        IFLT      DATCOR
089400010220     C*                  SETON                                        64
089500010220     C*                  MOVEL     MSG(16)       DSMSMS
089600010220     C*                  MOVEL     '1'           ERRL00
089700010220E   3C*                  ENDIF
089800010220E   2C*                  ENDIF
089900010220E   1C*                  ENDIF
090000970528     C*---
090100970528     C* DECORRENZA + SCADENZA
090200970528     C*---
090300970528     C* ACCAVALLAMENTO DATI
090400970528IF  1C     ERRL00        IFEQ      '0'
090500080110     C* Controllo solo x servizi diversi da tipo T&T
090600050118     C     vidisv        lookup    isvtt                                  22
090700050118     C                   if        *in22 = *off
090800991104     C                   MOVEL     VIDisv        KCEisv
090900050118     C                   move      ksuVID        KCEksu
091000040910     C     Ke2vss        SETLL     tivss02L
091100040910     C     Ke2vss        READE(N)  tivss02L                               99
091200970528DO  2C     *IN99         DOWEQ     *OFF
091300080110IF  3C                   IF        (DEPOP1<>'F06' AND DDEVID<>vssDDE) OR
091400080110     C                              DEPOP1= 'F06'
091500080110     C                   IF        (DDEVID>=vssDDE AND DDEVID<=vssDSC) OR
091600080110     C                             (DSCVID>=vssDDE AND DSCVID<=vssDSC) OR
091700080110     C                             (vssDDE>=DDEVID AND vssDSC<=DSCVID)
091800970528     C                   SETON                                        606162
091900970619     C                   SETON                                        6364
092000970528     C                   MOVEL     MSG(17)       DSMSMS
092100970528     C                   MOVEL     '1'           ERRL00
092200970528     C                   LEAVE
092300970528E   5C                   ENDIF
092400970528E   3C                   ENDIF
092500040910     C     Ke2vss        READE(N)  tivss02L                               99
092600970528E   2C                   ENDDO
092700970528E   1C                   ENDIF
092800050118     C                   endif
092900020214     C*
093000020214     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
093100020214     C     vidisv        lookup    isvkun                                 22
093200020214     C                   if        *in22 = *on
093300000131     C*--------------------
093400000131     C* Clienti figli collegati
093500000131     C*--------------------
093600000215     C                   MOVEL     'N'           $esikun                        *non esistono kun
093700001116     C                   CLEAR                   vidmsg
093800020212     C     ERRL00        IFEQ      '0'
093900020212     C     VIDtle        IFNE      *blanks                                      *c'è legame
094000000204     C                   MOVEL     vidtle        kkutle
094100000131     C                   MOVEL     *blanks       kkuazp
094200000204     C                   MOVE      vidksu        kkucop
094300000215     C     KEYkun        SETLL     tikun01l
094400000215     C     KEYkun        READE     tikun01l                               99
094500020212     C                   DOW       NOT *in99
094600020212     C     kunvat        IFEQ      *blanks
094700020212     C                   IF        kundsc <  ddevid  or                         *non valido
094800010522     C                             kundde >  dscvid
094900020212     C                   ELSE
095000060802     C                   IF        kundsc >= datcor
095100000215     C                   MOVEL     'S'           $esikun                        *esistono kun
095200001116     C                   LEAVE
095300060802     C                   ENDIF
095400020212     C                   ENDIF
095500020212     C                   ENDIF
095600000215     C     KEYkun        READE     tikun01l                               99
095700020212     C                   ENDDO
095800020212     C     $esikun       IFEQ      'N'                                          *non esistono kun
095900060802     C                   IF        kundsc >= datcor
096000020212     C                   MOVEL     des(1)        vidmsg
096100001116     C                   SETON                                        76        *attenzione
096200060802     C                   ENDIF
096300020212     C                   ENDIF
096400000215     C*
096500020212     C                   ELSE                                                   *non c'è legame
096600000215     C*
096700000215     C                   MOVE      vidksu        kkucop
096800000215     C     KE3kun        SETLL     tikun03l
096900000215     C     KE3kun        READE     tikun03l                               99
097000020212     C                   DOW       NOT *in99
097100020212     C     kunvat        IFEQ      *blanks
097200020212     C                   IF        kundsc <  ddevid  or                         *non valido
097300010522     C                             kundde >  dscvid
097400020212     C                   ELSE
097500000215     C                   MOVEL     'S'           $esikun                        *esistono kun
097600001116     C                   LEAVE
097700020212     C                   ENDIF
097800020212     C                   ENDIF
097900000808     C     KEYkun        READE     tikun01l                               99
098000020212     C                   ENDDO
098100020212     C     $esikun       IFEQ      'S'                                          *esistono kun
098200060802     C                   IF        kundsc >= datcor
098300001116     C                   MOVEL     des(2)        vidmsg
098400020212     C                   SETON                                        76        *attenzione
098500060802     C                   ENDIF
098600020212     C                   ENDIF
098700020212     C                   ENDIF
098800020212     C                   ENDIF
098900020211     C*--------------------
099000091027     C* Che nn sia cliente figlio
099100020211     C*--------------------
099200091117     C*    ERRL00        ifeq      '0'
099300091117     C*                  move(p)   vidksu        kkucof
099400091117     c*
099500091117     c* ciclo x tutti i legami d tipo "internet"
099600091117     c*                  z-add     1             jtle
099700091117     c*                  dow       jtle <= %elem(sktle)
099800091117     c*                  if        sktle(jtle) <> *blanks
099900091117     c*
100000091117     C*                  movel     sktle(jtle)   kkutle
100100091117     C*    ke6kun        setll     tikun06l
100200091117     C*    ke6kun        reade     tikun06l
100300091117     C*                  dow       not %eof(tikun06l)
100400091117     C*    kunvat        ifeq      *blanks                                      *annullati/sospesi
100500091117     C*                  if        kundsc <  ddevid  or                         *non valido
100600091117     C*                            kundde >  dscvid
100700091117     C*                  else                                                   *valido
100800091117     C*                  seton                                        61
100900091117     C*                  movel     MSG(27)       DSMSMS
101000091117     C*                  movel     '1'           ERRL00
101100091117     C*                  leave
101200091117     C*                  endif
101300091117     C*                  endif
101400091117     C*    ke6kun        reade     tikun06l
101500091117     C*                  enddo
101600091027     c*
101700091117     c*                  else
101800091117     c*                  leave
101900091117     c*                  endif
102000091117     c*
102100091117     c*                  add       1             jtle
102200091117     c*                  enddo
102300091117     C*                  endif
102400091117     C*
102500091117     C                   endif
102600000913     C*------------------------
102700010522     C* Verifico che in INS/COPIA non esista già il cliente per il tipo servizio DL (x dec+scad)
102800000913     C*------------------------
102900020212     C     ERRL00        IFEQ      '0'
103000010522     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') AND
103100010522     C                              VIDisv = 'DL'
103200000913     C                   MOVE(p)   vidksu        kceksu
103300001116     C                   MOVE(p)   vidisv        kceisv                         *stesso servizio
103400000913     C     KE2vss        SETLL     tivss02L
103500000913     C     KE2vss        READE     tivss02L                               99
103600020212     C     *IN99         DOWEQ     *OFF
103700020212     C     ddevid        IFGE      vssdde                                       *in decorrenza
103800001116     C     ddevid        ANDLE     vssdsc
103900001116     C                   MOVEL     MSG(24)       DSMSMS
104000000913     C                   MOVEL     '1'           ERRL00
104100001116     C                   LEAVE
104200020212     C                   ENDIF
104300000913     C     KE2vss        READE     tivss02L                               99
104400020212     C                   ENDDO
104500020212     C                   ENDIF
104600020212     C                   ENDIF
104700000731     C*--------------------
104800000731     C* codice strategi
104900000731     C*--------------------
105000020802     C                   MOVEL     VIDsun        depTIP            1
105100020802     C*
105200020802     C* Eseguo controllo compatibilità Codice STRATEGI rispotto al servizio indicato
105300020802     C     ERRL00        IFEQ      '0'
105400020802     C                   IF        depTIP = '9' and
105500020802     C                             §isvproc <> 'F'
105600020802     C                   SETON                                        6265
105700020802     C                   MOVEL     MSG(29)       DSMSMS
105800020802     C                   MOVEL     '1'           ERRL00
105900020802     C                   ENDIF
106000020802     C                   IF        depTIP <> '9' and
106100020802     C                             §isvproc <> 'S'
106200020802     C                   SETON                                        6265
106300020802     C                   MOVEL     MSG(29)       DSMSMS
106400020802     C                   MOVEL     '1'           ERRL00
106500020802     C                   ENDIF
106600020802     C                   ENDIF
106700020802     C*
106800020802     C* Eseguo controllo esistenza Codice STRATEGI solo se "sun" nn inizia x '9'
106900020212     C     ERRL00        IFEQ      '0'
107000020802     C                   IF        depTIP <> '9'
107100000731     C                   EXSR      scanfile
107200020212     C                   ENDIF
107300020802     C                   ENDIF
107400010605     C*
107500010605     C* SE QUI TUTTO OK E TIPO SERVIZIO = 'TT' ESEGUO CONTROLLO ED EVENTUALE INSERIMNETO
107600010605     C* ANCHE DEL RECORD PER GLI ORM DA INTERNET NELL'ABILITAZIONE TIPI FILE X CLIENTE (TIVIR00F)
107700010608     C     ERRL00        IFEQ      '0'
107800010608     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') and
107900010608     C                             VIDisv = 'TT'
108000010626 ORM C*                  EXSR      DECVIR
108100010605     C                   ENDIF
108200010605     C                   ENDIF
108300090804     C*--------------------
108400100217     C* SUBK / tipo servizio
108500090804     C*--------------------
108600090804     C     ERRL00        IFEQ      '0'
108700090804     C                   IF        VIDSUBK <> *blanks AND VIDISV <> 'TT'
108800090804     C                   MOVEL     '1'           ERRL00
108900090804     C                   SETON                                        67
109000090804     C                   MOVEL     MSG(30)       DSMSMS
109100090804     C                   ENDIF
109200090804     C                   ENDIF
109300100217     C*--------------------
109400100217     C* controlli specifici x servizio
109500100217     C*--------------------
109600100217     C     ERRL00        IFEQ      '0'
109700100217     C                   z-add     1             jISV              3 0
109800100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
109900100217     C                   if        *in22 = *on
110000100217     C                   if        isvCTRL(jISV) <> *blanks
110100100217     C                   select
110200100913     C* - 0001
110300100322     C                   when      isvCTRL(jISV) = '0001'
110400120327     C*
110500120328     C* se cliente annullato o bloccato per motivi di mancato pagamento
110600120327     C                   IF        ACOFlg <> *blanks
110700130314     C                             or ACOAbl <> *blanks
110800120327     C                   SETON                                        61
110900120327     C                   EVAL      ErrL00 = '1'
111000120327     C                   MOVEL     MSG(37)       DSMSMS
111100120327     C                   ENDIF
111200120327     C*
111300120327     C     ERRL00        IFEQ      '0'
111400100217     C                   eval      kceksu = vidksu
111500100217     C                   eval      kceisv = 'TT'
111600100217     C                   eval      kcesun = vidsun
111700100217     C     KEYvss02_C    chain     tivss02L
111800100217     C                   if        %found(tivss02l) and
111900100217     C                             vssdde <= datcor and
112000100217     C                             vssdsc >  datcor
112100100322     C*
112200100322     C     FUNKEY        IFEQ      '06'
112300100406     C*
112400100406      /FREE
112500100406
112600100406              Ub§3EWupd_Upd (VIDKSU : VIDSUN : 'A' : *off );
112700100406
112800100406
112900100406      /END-FREE
113000100322     C*
113100100406     C                   ENDIF
113200100217     C                   else
113300100217     C                   MOVEL     '1'           ERRL00
113400100217     C                   SETON                                        62
113500100913     C                   MOVEL     MSG(31)       DSMSMS
113600100217     C                   endif
113700120327     C                   endif
113800101006     C* Infine rileggo il record "originale"
113900101006     C                   eval      kceksu = vidksu
114000101006     C                   eval      kceisv = vidisv
114100101006     C                   eval      kcesun = vidsun
114200101006     C     KEYvss02_C    chain     tivss02L
114300100913     C*
114400100217     C                   endsl
114500100217     C                   endif
114600100217     C                   endif
114700100217     C                   ENDIF
114800100913     C*--------------------
114900100913     C* Password FTP
115000100913     C*--------------------
115100100913     C     ERRL00        IFEQ      '0'
115200100913     C                   IF        *IN80 = *on
115300100913     C                   IF        VIDPWDFTP = *blanks
115400100913     C                   MOVEL     '1'           ERRL00
115500100913     C                   SETON                                        68
115600100913     C                   MOVEL     MSG(32)       DSMSMS
115700100913     C                   ELSE
115800100913     C*
115900100913     C* Password => verifica caratteri consentiti
116000100913     C                   Z-ADD     1             pos               2 0
116100100913     C                   FOR       pos = 1 to %len(%trim(VIDPWDFTP))
116200100913     C                   IF        %lookup(%subst(%trim(VIDPWDFTP):pos:1)
116300100913     C                             :valAry) = *zeros
116400100913     C                   MOVEL     '1'           ERRL00
116500100913     C                   SETON                                        68
116600100913     C                   MOVEL     MSG(33)       DSMSMS
116700100913     C                   ENDIF
116800100913     C                   ENDFOR
116900100913     C*
117000100913     C                   IF        ERRL00 <> '1'
117100100913     C                   IF        %len(%trim(VIDPWDFTP)) < 8
117200100913     C                   MOVEL     '1'           ERRL00
117300100913     C                   SETON                                        68
117400100913     C                   MOVEL     MSG(34)       DSMSMS
117500100913     C                   ENDIF
117600100913     C                   ENDIF
117700100913     C                   ENDIF
117800100913     C                   ENDIF
117900100913     C                   ENDIF
118000100914     C*--------------------
118100100914     C* Verifica Servizio / "F18"
118200100914     C*--------------------
118300100914     C     ERRL00        IFEQ      '0'
118400100914     C                   IF        VIDISV <> 'IE' and FUNKEY = '18'
118500100914     C                   MOVEL     '1'           ERRL00
118600100914     C                   SETON                                        68
118700100914     C                   MOVEL     MSG(35)       DSMSMS
118800100914     C                   ENDIF
118900100914     C                   ENDIF
119000060214     C*
119100100217     C* Forzatura necessaria
119200060214     C                   IF        KNMUS = 'EDPFG'
119300060214     C                   SETOFF                                       9091
119400060214     C                   ENDIF
119500000731     C*
119600970528     C                   ENDSR
119700010605     C*------------------------------------------------------------------------*
119800010608     C* DECVIR - CONTROLLA CAMPI DA INSERIRE NEL FILE "REGOLE X CLIENTE" (TIVIR00F)
119900010605     C*------------------------------------------------------------------------*
120000010605     C     DECVIR        BEGSR
120100010608     C*
120200010608     C                   MOVEL     '0'           ERRL01
120300010605     C*
120400010605     C*--------------------
120500010605     C* INSERIMENTO -> RECORD INESISTENTE
120600010605     C*--------------------
120700010608     C     ERRL01        IFEQ      '0'
120800010605     C                   MOVE(P)   VIDksu        KCEksc
120900010605     C                   MOVEL     'TO'          KCEtip
121000010605     C                   Z-ADD     ddeVID        KCEdti
121100010605     C     KEYvi2        CHAIN(N)  tivir03L                           99
121200010605     C     *IN99         IFEQ      *ON
121300010605     C* DECORRENZA + SCADENZA  (ACCAVALLAMENTO DATI)
121400010605     C     KEYvir        SETLL     tivir03L
121500010605     C     KEYvir        READE(N)  tivir03L                               99
121600010605     C     *IN99         DOWEQ     *OFF
121700010605     C     ddeVID        IFGE      virdti                                       *DECORRENZA
121800010605     C     ddeVID        ANDLE     virdtf
121900010605     C     dscVID        ORGE      virdti                                       *SCADENZA
122000010605     C     dscVID        ANDLE     virdtf
122100010605     C     virdti        ORGE      ddeVID                                       *DECORRENZA
122200010605     C     virdtf        ANDLE     dscVID
122300010605     C                   MOVEL     MSG(25)       DSMSMS
122400010608     C                   MOVEL     '1'           ERRL01
122500010605     C                   LEAVE
122600010605     C                   ENDIF
122700010605     C     KEYvir        READE(N)  tivir03L                               99
122800010605     C                   ENDDO
122900010605     C                   ELSE
123000010605     C                   MOVEL     MSG(26)       DSMSMS
123100010608     C                   MOVEL     '1'           ERRL01
123200010605     C                   ENDIF
123300010605     C                   ENDIF
123400010605     C*
123500010605     C                   ENDSR
123600010605     C*------------------------------------------------------------------------*
123700010605     C* SCANFILE - Routine di scansione del file degli UTENTI di STRATEGI
123800010605     C*------------------------------------------------------------------------*
123900010605     C     SCANFILE      BEGSR
124000000719     C*
124100000719     C                   open      sgius
124200000719     C*
124300000719     C                   eval      chk = '1'
124400000719     C                   add       1             giro
124500130611     C     vidsun        chain(e)  sgius
124600130611     C                   if        %found(sgius)
124700130611     C                   eval      chk = '0'
124800130611     C                   endif
124900000719     C*
125000000719     C                   if        chk = '1'
125100000719     C                   movel     MSG(21)       DSMSMS                         *MESSAGGIO
125200000719     C                   endif
125300000719     C*
125400000719     C                   close     sgius
125500000719     C*
125600010605     C                   ENDSR
125700991008     C*------------------------------------------------------------------------*
125800000807     C* Cntisv - CONTROLLA codice tipo servizio a cui il cliente è abilitato
125900991008     C*------------------------------------------------------------------------*
126000001116     C     cntisv        begsr
126100001116     C                   clear                   TIBS02DS
126200001116     C*
126300991104     C     '?'           SCAN      Vidisv                                 30
126400001116     C*  Ricerca
126500001116     C                   eval      t02tla = 'L'
126600001116    1C                   IF        *IN30 = *ON
126700001116     C                   movel     'R'           t02mod
126800001116     C                   else
126900001116     C                   movel     'C'           t02mod
127000001116     C                   movel(p)  vidisv        t02ke1
127100001116     C                   end
127200001116     C                   movel     'ISV'         t02cod
127300001116     C                   movel     knsif         t02sif
127400991008     C                   CALL      'TIBS02R'
127500001116     C                   parm                    KPJBA
127600001116     C                   parm                    TIBS02DS
127700001116     C
127800001116     C*
127900991008    2C                   IF        T02err = *blanks
128000991104     C                   movel     T02ke1        Vidisv
128100020802     C                   movel(p)  T02uni        DSISV
128200020802     C                   movel     §isvdesc      vddisv
128300001116     C                   else
128400001116     C                   movel     '1'           errl00
128500001116     C                   seton                                        62
128600991104     C                   MOVEL     MSG(9)        DSMSMS
128700991008     C                   ENDIF
128800001116     C
128900001116     C                   endsr
129000000807     C*------------------------------------------------------------------------*
129100991104     C* Cnttle - CONTROLLA codice tipo legame
129200991104     C*------------------------------------------------------------------------*
129300001116     C     cnttle        begsr
129400001116     C                   clear                   TIBS02DS
129500001116     C*
129600991104     C     '?'           SCAN      Vidtle                                 30
129700001116     C*  Ricerca
129800001116     C                   eval      t02tla = 'L'
129900991104    1C                   IF        *IN30 = *ON
130000001116     C                   movel     'R'           t02mod
130100001116     C                   else
130200001116     C                   movel     'C'           t02mod
130300001116     C                   movel(p)  vidtle        t02ke1
130400001116     C                   end
130500991104     C                   movel     'TLE'         t02cod
130600001116     C                   movel     knsif         t02sif
130700001116     C                   CALL      'TIBS02R'
130800001116     C                   parm                    KPJBA
130900001116     C                   parm                    TIBS02DS
131000001116     C
131100001116     C*
131200991104    2C                   IF        T02err = *blanks
131300000807     C                   exsr      chkaut
131400000807     C                   if        errl00 = *blanks
131500991104     C                   movel     T02ke1        Vidtle
131600091027     C                   movel     T02uni        dstle
131700991104     C                   movel     §tledesc      Vddtle
131800000807     C                   endif
131900000807     C                   else
132000000807     C                   movel     '1'           errl00
132100000807     C                   seton                                        66
132200000807     C                   MOVEL     MSG(12)       DSMSMS
132300991104     C                   ENDIF
132400000807     C
132500000807     C                   endsr
132600000807     C*------------------------------------------------------------------------*
132700000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
132800000807     C*------------------------------------------------------------------------*
132900000807     C     CHKAUT        BEGSR
133000000807     C*
133100020903     C* VERIFICO CHE L'UTENTE CHE EFFETTUA LA RICHIESTA SIA UN EDP*
133200020903     C                   IF        %subst(KNMUS:1:3) = 'EDP'
133300020903     C                   ELSE
133400020903     C                   MOVEL     '1'           ERRL00
133500020903     C                   MOVEL     MSG(23)       DSMSMS
133600020903     C                   ENDIF
133700000807     C*
133800000807     C                   ENDSR
133900970528     C*------------------------------------------------------------------------*
134000991105     C* CHKksu - CONTROLLA codice padre figlio
134100970528     C*------------------------------------------------------------------------*
134200991105     C     CHKksu        BEGSR
134300970528     C*
134400991105     C     *LIKE         DEFINE    VIDksu        Wksu
134500991105     C     *LIKE         DEFINE    VDDksu        WDksu
134600001116     C                   MOVE      *zeros        Wksc
134700021114     C                   MOVEL     'S'           flgKSU            1
134800970528     C*
134900970528     C* RICHIESTO ELENCO
135000970528     C                   SETOFF                                           28
135100991105     C     '?'           SCAN      Wksu                                   28
135200970528IF  1C     *IN28         IFEQ      *ON
135300991008     C* RICHIESTO ELENCO CODICI
135400021114     C                   MOVEL     'N'           flgKSU
135500001116     C* CLIENTI
135600991008     C                   CLEAR                   DSXC
135700020924     C                   Z-ADD     DUTKCI        DXCCAP
135800991008     C                   CALL      'XCLIR'
135900991008     C                   PARM                    DSXC
136000991008     C* SCOMPATTA
136100991008     C                   MOVEA     DXCKSC        KSA                        ALF
136200991008DO  2C     1             DO        30            J                 3 0
136300991008     C                   MOVE      KSA(J)        DSKSA                      F
136400991008     C                   Z-ADD     DSKSN         KSC(J)                     M
136500991008E   2C                   ENDDO
136600991011     C                   MOVEL     KSC(1)        WKSC              7
136700060807     C                   MOVEL     *all'0'       WKSU
136800060807     C                   MOVE      WKSC          WKSU
136900991011     C                   MOVEL     '1'           $TESVD            1        TA
137000060807E   1C                   ENDIF
137100991008     C*
137200991008     C* CODICE CLIENTE NUMERICO
137300021114IF  1C     WKSC          IFLT      *ZEROS
137400021114     C                   MOVEL     '1'           WERR              1
137500021114X   1C                   ELSE
137600021114     C*
137700970528     C* CONTROLLA ESISTENZA CODICE
137800021114     C                   MOVEL     'S'           flgKSU
137900991210     C                   MOVE      Wksu          wcli              7 0
138000001116     C                   EXSR      dectib
138100021114E   1C                   ENDIF
138200970528     C*
138300970528     C                   ENDSR
138400991210     C*------------------------------------------------------------------------*
138500001116     C* DECTIB - Controlla esistenza codice cliente
138600991210     C*------------------------------------------------------------------------*
138700001116     C     DECTIB        BEGSR
138800001116     C*
138900010913     C     VIDksu        CHAIN     tianv01l
139000010913     C                   IF        %found(tianv01l)
139100010913     C                   MOVEL(p)  anvdes        WDKsu
139200010913     C                   ELSE
139300050909     C*
139400050909     C* SOLO CARATTERI NUMERICI
139500050909     C   28              MOVE      *all'0'       wksu
139600050909     C   28              MOVE      wksc          wksu
139700050909     C                   SETOFF                                       28
139800050909     C                   TESTN                   Wksu                 28
139900050909IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
140000120327     C                   MOVEL     '1'           ERRL00
140100050909X   2C                   ELSE                                                   *E'NUMERO DA TESTN
140200050909     C                   MOVE      Wksu          W001              1
140300050909IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
140400050909     C                   MOVEL     '1'           ERRL00
140500050909X   3C                   ELSE                                                   *NUMERO
140600991210     C                   CLEAR                   BS69DS
140700991210     C                   CLEAR                   ACODS
140800991210     C                   MOVEL     KNSIF         I69SIF
140900991210     C                   Z-ADD     WCLI          I69KAC
141000991210     C                   CALL      'TIBS69R'
141100991210     C                   PARM                    BS69DS
141200991210     C                   PARM                    ACODS
141300991210     C                   PARM                    INDDS
141400991210     C                   PARM                    CLPDS
141500991210     C                   PARM                    CLSDS
141600991210     C     O69ERR        IFNE      '1'
141700991210     C                   MOVEL     ACORAG        wdksu
141800991210     C                   ELSE
141900000728     C                   MOVEL     *ALL'*'       Wdksu
142000001116     C                   MOVE      '1'           errl00
142100021114     C                   MOVEL     'N'           flgKSU
142200001116     C                   ENDIF
142300050909E   3C                   ENDIF
142400050909E   2C                   ENDIF
142500991210     C                   ENDIF
142600001116     C*
142700001116     C                   ENDSR
142800991210     C*------------------------------------------------------------------------*
142900991210     C* FINSR - OPERAZIONI FINALI
143000991210     C*------------------------------------------------------------------------*
143100991210     C     FINSR         BEGSR
143200991210     C*
143300991210     C* CHIUDE I FILE DEI PGM CHIAMATI
143400991210     C                   CLEAR                   bs69ds
143500991210     C                   CLEAR                   acods
143600991210     C                   CLEAR                   indds
143700991210     C                   CLEAR                   clpds
143800991210     C                   CLEAR                   clsds
143900991210     C                   MOVEL     'C'           i69tla
144000991210     C                   MOVEL     knsif         i69sif
144100991210     C                   CALL      'TIBS69R'
144200991210     C                   PARM                    bs69ds
144300991210     C                   PARM                    acods
144400991210     C                   PARM                    indds
144500991210     C                   PARM                    clpds
144600991210     C                   PARM                    clsds
144700991210     C*
144800991210     C                   ENDSR
144900970528     C*------------------------------------------------------------------------*
145000970528     C* CHKDA8 - CONTROLLA LA DATA (8)
145100970528     C*------------------------------------------------------------------------*
145200970528     C     CHKDA8        BEGSR
145300970528     C*
145400970528     C     *LIKE         DEFINE    VIDDDE        WDA8
145500970528     C     *LIKE         DEFINE    VIDDDE        DA8W
145600970528     C*
145700970528     C                   Z-ADD     WDA8          G08DAT
145800970528     C                   Z-ADD     *ZEROS        G08INV
145900970528     C                   MOVEL     '0'           G08ERR
146000970528     C                   CALL      'XSRDA8'
146100970528     C                   PARM                    WLBDA8
146200970528     C                   Z-ADD     G08DAT        WDA8
146300970528     C                   Z-ADD     G08INV        DA8W
146400970528IF  1C     G08ERR        IFEQ      '1'
146500970528     C                   MOVEL     '1'           ERRL00
146600970528E   1C                   ENDIF
146700970528     C*
146800970528     C                   ENDSR
146900970526     C*------------------------------------------------------------------------*
147000970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
147100970526     C*------------------------------------------------------------------------*
147200970526     C     IREVID        BEGSR
147300970526     C* CAMPI
147400991104     C                   MOVEL     vsstle        VIDtle
147500991104     C                   MOVEL     vssisv        VIDisv
147600991104     C                   MOVEL     vssazp        VIDazp
147700991105     C                   MOVEL     vsssun        VIDsun
147800001116     C                   MOVE      vssksu        VIDksu
147900090804     C                   MOVEL     vssDTA        DVSSDTA
148000090804     C                   MOVEL     §VSSSUBK      VIDSUBK
148100991104     C                   Z-ADD     vssDDE        G08INV
148200970528     C                   Z-ADD     *ZEROS        G08DAT
148300970528     C                   MOVEL     '3'           G08ERR
148400970528     C                   CALL      'XSRDA8'
148500970528     C                   PARM                    WLBDA8
148600970528     C                   Z-ADD     G08DAT        VIDDDE
148700991104     C                   Z-ADD     vssDSC        G08INV
148800970528     C                   Z-ADD     *ZEROS        G08DAT
148900970528     C                   MOVEL     '3'           G08ERR
149000970528     C                   CALL      'XSRDA8'
149100970528     C                   PARM                    WLBDA8
149200051013     C                   Z-ADD     G08DAT        VIDDSC
149300051013     C*
149400051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
149500051013     C                   IF        DEPOP1 = 'F06' OR
149600051013     C                             DEPOP1 = 'O03' OR
149700051013     C                             DEPOP1 = 'O07'
149800051013     C                   EVAL      VIDDDE = DATCORINV
149900060726     C                   EVAL      VIDDSC = 31122039
150000051013     C                   ENDIF
150100970527     C*
150200970526     C                   ENDSR
150300970526     C*------------------------------------------------------------------------*
150400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
150500970526     C*------------------------------------------------------------------------*
150600970526     C     AZZVID        BEGSR
150700970526     C* CAMPI
150800000316     C*---               MOVEL     *BLANKS       VIDtle
150900991104     C                   MOVEL     *BLANKS       VIDisv
151000100913     C                   MOVEL     *BLANKS       VIDpwdftp
151100000316     C*---               MOVEL     *BLANKS       VIDazp
151200000316     C*---               Z-ADD     *ZEROS        VIDDDE
151300000316     C*---               Z-ADD     *ZEROS        VIDDSC
151400000316     C*---               move      *ZEROS        VIDksu
151500000316     C*---               move      *ZEROS        VIDsun
151600970526     C* DECODIFICHE
151700000316     C*---               MOVEL     *BLANKS       VDDtle
151800000215     C                   MOVEL     *BLANKS       VIDmsg
151900000316     C*---               MOVEL     *BLANKS       VDDksu
152000000316     C                   MOVEL     *BLANKS       VDDisv
152100970526     C*
152200970526     C                   ENDSR
152300970526     C*------------------------------------------------------------------------*
152400991104     C* IMPvss - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
152500970526     C*------------------------------------------------------------------------*
152600991104     C     IMPvss        BEGSR
152700970526     C*
152800991104     C                   MOVE      VIDazp        vssazp                         *CHIAVE
152900001116     C                   MOVE      ksuVID        vssksu
153000090804     C                   MOVEL     VIDSUBK       §VSSSUBK
153100090831     C                   EVAL      vssDTA = DVSSDTA
153200001116     C                   MOVE      VIDtle        vsstle
153300090714     C                   MOVE      VIDisv        vssISV
153400991104     C                   Z-ADD     DDEVID        vssDDE
153500991104     C                   Z-ADD     DSCVID        vssDSC
153600970526     C*
153700970526     C                   ENDSR
153800010605     C*------------------------------------------------------------------------*
153900010605     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE DELLE ABILITAZIONI CLIENTI/TIPI FILE (TIVIR)
154000010605     C*------------------------------------------------------------------------*
154100010605     C     IMPvir        BEGSR
154200010605     C*
154300010605     C                   MOVE      ksuVID        virksc
154400010605     C                   MOVE      'TO'          virtip
154500010605     C                   MOVE      'TT'          virfi1
154600010605     C                   MOVE      001           virseq
154700010605     C                   MOVE      999           virpoi
154800010605     C                   EVAL      virdes = 'ORM DA INTERNET'
154900010605     C                   MOVEL     'G '          virfi2
155000010605     C                   Z-ADD     DDEVID        virdti
155100010605     C                   Z-ADD     DSCVID        virdtf
155200010605     C*
155300010605     C                   ENDSR
155400970526     C*------------------------------------------------------------------------*
155500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
155600970526     C*------------------------------------------------------------------------*
155700970526     C     IMPFUN        BEGSR
155800970526     C*
155900970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
156000970526IF  1C     DEPOP1        IFEQ      'O05'
156100970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
156200970526     C                   MOVEL     'N'           FUDATT
156300970526     C                   MOVEL     FUDRIG        FUD(6)
156400970526E   1C                   ENDIF
156500100914     C*
156600100914     C* SE NO INSERIMENTO e NO COPIOA => NO "F18"
156700100914     C                   IF        DEPOP1 <> 'F06' AND
156800100914     C                             DEPOP1 <> 'O03'
156900111216     C* NO F18 anche se l'AS400 non è SETRAS
157000111216     C                             or  AS400_ok = *off
157100100914     C                   MOVEL     FUD(18)       FUDRIG                         *AGGIORNAMENTO
157200100914     C                   MOVEL     'N'           FUDATT
157300100914     C                   MOVEL     FUDRIG        FUD(18)
157400100914     C                   ENDIF
157500970526     C*
157600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
157700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
157800970526     C                   EXSR      FUNGES
157900970526     C*
158000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
158100970526     C*---
158200970526     C* 90 -> PROTEZIONE CHIAVE
158300970526     C* 91 -> PROTEZIONE TOTALE
158400030704     C* 92 -> PROTEZIONE PARZIALE
158500970526     C*---
158600970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
158700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
158800970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
158900970526     C                   SETOFF                                       91
159000970526E   2C                   ENDIF
159100970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
159200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
159300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
159400970526E   2C                   ENDIF
159500970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
159600970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
159700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
159800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
159900970526     C                   SETON                                        91
160000970526E   2C                   ENDIF
160100970526X   1C                   ELSE                                                   *I N T E R R O G A
160200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
160300970526     C                   SETON                                        91
160400970526E   1C                   ENDIF
160500030704     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
160600030704     C                   SETON                                        92        *SPROTEZIONE SCAD.
160700030704     C                   ENDIF
160800970526     C*
160900970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
161000970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
161100991104     C                   MOVEL     s20isv        KCEisv
161200001116     C                   MOVE      s20sun        KCEsun
161300991104     C     KEYvss        CHAIN     tivss01L                           9998
161400001116IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
161500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
161600970526     C                   MOVEL     MSG(6)        DSMSMS
161700970526X   2C                   ELSE
161800970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
161900980128IF  3C     DEPOP1        IFEQ      'O05'
162000001116     C     DEPOP1        OREQ      'O02'
162100101022     C*
162200101022     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
162300101022     C     vssISV        lookup    isvftp                                 80
162400101022     C                   IF        *IN80
162500111216     C                             and AS400_ok = *on
162600101022      /FREE
162700101022
162800101022         if UBBVSQL01_RtvUtente(
162900101022                         pInConn
163000101022                       : %subst(vssKSU:2:7)
163100101022                       : pOutNumRec
163200101022                       : pOutUsrPwd
163300101022                       : pOutUsrPath
163400101022                       : pOutUsrDesc
163500101022                       : pOutUsrFAnn
163600101022                       : pOutUsrEmail
163700101022                       : pOutUsrNote
163800101022                       : pOutUsrFList) = 0;
163900101022              if pOutNumRec = 1;
164000101022                 VIDPWDFTP = pOutUsrPwd;
164100101022              endif;
164200101022         endif;
164300101022
164400101022      /END-FREE
164500101022     C
164600101022     C                   ENDIF
164700101022     C*
164800101022     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
164900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
165000980128E   3C                   ENDIF
165100970526E   2C                   ENDIF
165200970526X   1C                   ELSE                                                   *INSERIMENTO
165300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
165400970526E   1C                   ENDIF
165500970526     C*
165600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
165700970526IF  1C     DEPOP1        IFEQ      'O02'
165800970526     C                   MOVEL     DEC(1)        VIDFUN
165900970526E   1C                   ENDIF
166000991014IF  1C     DEPOP1        IFEQ      'O03'
166100991014     C                   MOVEL     DEC(6)        VIDFUN
166200991014E   1C                   ENDIF
166300970526IF  1C     DEPOP1        IFEQ      'O04'
166400970526     C                   MOVEL     DEC(2)        VIDFUN
166500970526E   1C                   ENDIF
166600970526IF  1C     DEPOP1        IFEQ      'O05'
166700970526     C                   MOVEL     DEC(3)        VIDFUN
166800970526E   1C                   ENDIF
166900970526IF  1C     DEPOP1        IFEQ      'O07'
167000970526     C                   MOVEL     DEC(4)        VIDFUN
167100970526E   1C                   ENDIF
167200970526IF  1C     DEPOP1        IFEQ      'F06'
167300970526     C                   MOVEL     DEC(5)        VIDFUN
167400970526E   1C                   ENDIF
167500970526     C*
167600970526     C                   ENDSR
167700020212     C*--------------------------------------------------------------------------------------------*
167800020212     C* CARTAB - carica le tabelle occorrenti
167900020212     C*--------------------------------------------------------------------------------------------*
168000020212     C     CARTAB        BEGSR
168100020212     C*
168200020212     C* Tipi servizi soggetti a controllo unificazioni
168300020212     C                   z-add     *zeros        x                 3 0
168400050118     C                   z-add     *zeros        y                 3 0
168500100217     C                   z-add     *zeros        k                 3 0
168600100913     C                   z-add     *zeros        ww                3 0
168700020212     C                   eval      tbecod = 'ISV'
168800091027     C     tbecod        setll     tntbe01l
168900091027     C     tbecod        reade     tntbe01l
169000020212     C                   dow       not %eof(tntbe01l)
169100020212     C                   movel     tbeuni        dsisv
169200020212     C                   if        §isvuni  = 'S'
169300020212     C                   add       1             x
169400020212     C                   eval      isvkun(x) = tbeke1
169500020212     C                   endif
169600080110     C                   if        §isvrgr  = 'T'
169700050118     C                   add       1             y
169800050118     C                   eval      isvtt(y) = tbeke1
169900050118     C                   endif
170000100913     C                   if        §isvftp  = 'F'
170100100913     C                   add       1             ww
170200100913     C                   eval      isvftp(ww) = tbeke1
170300100913     C                   endif
170400100217     C                   add       1             k
170500100217     C                   eval      isvKEYS(k)   = tbeke1
170600100217     C                   eval      isvCTRL(k)   = §ISVCTRL
170700101006     C                   eval      isvPGMAFT(k) = §ISVPGMSRV
170800091027     C     tbecod        reade     tntbe01l
170900020212     C                   enddo
171000091027     c*---
171100091027     c* tipi legami "internet"
171200091027     c*---
171300091027     c                   z-add     *zeros        jtle
171400091027     c                   movel     'TLE'         tbecod
171500091027     c     tbecod        setll     tntbe01l
171600091027     c     tbecod        reade     tntbe01l
171700091027     c                   dow       not %eof(tntbe01l)
171800091027     c                   if        tbeatb = *blanks
171900091027     c                   eval      dstle = tbeuni
172000091027     c* solo legami d tipo "internet"
172100091027     c                   if        §TLEGRP = 'W'
172200091027     c                   add       1             jtle
172300091027     c                   eval      sktle(jtle) = tbeke1
172400091027     c                   endif
172500091027     c                   endif
172600091027     c     tbecod        reade     tntbe01l
172700091027     c                   enddo
172800020212     C*
172900020212     C                   ENDSR
173000020924     C*--------------------------------------------------------------------------------------------*
173100020924     C* REPERISCE I DATI UTENTE
173200020924     C*--------------------------------------------------------------------------------------------*
173300020924     C     REPDATIUTE    BEGSR
173400020924     C*
173500020924     C* INIZIALIZZA VARIABILI DI WRK
173600020924     C                   CLEAR                   TIBS34DS
173700020924     C                   CLEAR                   AZUTEDS
173800020924     C                   CLEAR                   DDATIUTE
173900020924     C*
174000020924     C     *DTAARA       DEFINE    §azute        azuteds
174100020924     C     *DTAARA       DEFINE    §datiute      ddatiute
174200020924     C                   IN(E)     *DTAARA
174300020924if  1C                   IF        %Error
174400020924     c                   EVAL      I34Tla = 'L'
174500020924     C                   CALL      'TIBS34R'
174600020924     C                   PARM                    Tibs34Ds
174700020924     C                   IN        *DTAARA
174800020924e   1C                   ENDIF
174900020924     C*
175000020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
175100020924     C                   MOVEL(P)  RSUT          DSFIRS           20
175200020924     C*
175300020924     C                   ENDSR
175400040422     C*------------------------------------------------------------------------*
175500040422     C* EXESGI - GESTIONE PARAMETRI PARTICOLARI X UTENTE STRATEGI
175600040422     C*------------------------------------------------------------------------*
175700040422     C     EXESGI        BEGSR
175800040422     C*
175900040422     C* Inizializzo il flag che indica l'autorizzazione speciale da attribuire al servizio
176000040422     C                   movel     *blanks       wABILSGI          1
176100040422     C*
176200040422     C* Innanzitutto verifico se il servizio è d tipo STRATEGI e quale autorizzazione speciale
176300040422     C* prevede
176400040422     C                   eval      KTBcod = 'ISV'
176500040422     C                   eval      KTBky1 = VIDISV
176600040422     C     KEYtbe        chain     tntbe01l
176700040422     C                   if        %found(tntbe01l)
176800040422     C                   eval      DSISV = tbeUNI
176900040422     C                   if        §ISVPROC = 'S' AND
177000040422     C                             §ISVSGI1 <> *blanks
177100040422     C                   eval      wABILSGI = §ISVSGI1
177200040422     C                   endif
177300040422     C                   endif
177400040422     C*
177500040422     C* Se x il servizio è prevista una particolare abilitazione => procedo
177600040422     C                   if        wABILSGI <> *blanks
177700040422     C* Costrisco la stringa d comando base x impostare i parametri adeguati sull'utente STRATEGI
177800040422     C                   eval      CMD = 'STRATEGI/CHGSGIUSR ' +
177900040422     C                                   VIDSUN + '  ' +
178000040422     C                                   'ALWWEB(' + api + '*YES' + api + ') '
178100040422     C*
178200040422     C* Gestisco i vari parametri diversi da abilitare a seconda del servizio
178300040422     C                   if        wABILSGI = 'U'                               * UPLOAD
178400040422     C                   eval      CMD = %trim(CMD) + ' ' +
178500040422     C                             'ALWRCVFILE(' + api + '*YES' + api + ') '
178600040422     C                   endif
178700040422     C                   if        wABILSGI = 'D'                               * DOWNLOAD
178800040422     C                   eval      CMD = %trim(CMD) + ' ' +
178900040422     C                                'ALWFILE(' + api + '*YES' + api + ') '
179000040422     C                   endif
179100040422     C                   if        wABILSGI = 'S'                               * OUTQ
179200040422     C                   eval      CMD = %trim(CMD) + ' ' +
179300040422     C                              ' ALWSPOOL(' + api + '*YES' + api + ') '
179400040422     C                   endif
179500040422     C*
179600040422     C* Esegue comando remoto su AS777
179700040422     C                   call(e)   'TIS730C'
179800040422     C                   parm                    cmd
179900040422     C*
180000040422     C                   endif
180100040422     C*
180200040422     C                   ENDSR
180300090610     C*------------------------------------------------------------------------*
180400090610     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
180500090610     C*------------------------------------------------------------------------*
180600090610     C     CHKSGI        BEGSR
180700090714      /FREE
180800090714               if UBUTISGI_Init = 0;
180900090714               endif;
181000090714      /END-FREE
181100090611     C*
181200090610      /FREE
181300090610               pInSUN = vidSUN;
181400090610               pInKSU = vidKSU;
181500090611               pInRGR = 'S';
181600090610
181700090610               if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
181800090610               endif;
181900090610
182000090610               if pOutEsito = '0';
182100090610                  if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
182200090610                  endif;
182300110503               else;
182400090611                  if UBUTISGI_EnableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
182500090611                  endif;
182600090611               endif;
182700090611      /END-FREE
182800090611     C
182900090923     C
183000090714      /FREE
183100090714               if UBUTISGI_Finalize = 0;
183200090714               endif;
183300090714      /END-FREE
183400090714     C*
183500090610     C                   ENDSR
183600101022
183700101022
183800101022
183900101022     c*------------------------------------------------------------------------*
184000101022     c* opnCONN - Apertura connessione
184100101022     c*------------------------------------------------------------------------*
184200101022     c     OPNCONN       BEGSR
184300101022      /FREE
184400101022
184500101022        if not *in40;
184600101022           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
184700101022              pInConn = pOutConn;
184800101022           else;
184900101022              *in40 = *on;
185000101022              s725_§21_ERRFLG = 'E';
185100101022              s725_§21_ERRMSG = 'Errore BartVAS: fatal error ';
185200101022           endif;
185300101022        endif;
185400101022
185500101022      /END-FREE
185600101022      *
185700101022     c                   ENDSR
185800101022     c*------------------------------------------------------------------------*
185900101022
186000101022
186100101022
186200101022     c*------------------------------------------------------------------------*
186300101022     c* clsCONN - Chiusura connessione
186400101022     c*------------------------------------------------------------------------*
186500101022     c     CLSCONN       BEGSR
186600101022      /FREE
186700101022
186800101022           if UBBVSQL01_Disconnect(pInConn) = 0;
186900101022           else;
187000101022              *in40 = *on;
187100101022           endif;
187200101022
187300101022      /END-FREE
187400101022      *
187500101022     c                   ENDSR
187600101022     c*------------------------------------------------------------------------*
187700101022
187800101022
187900101022
188000970526     C*------------------------------------------------------------------------*
188100970526     C* *INZSR - OPERAZIONI INIZIALI
188200970526     C*------------------------------------------------------------------------*
188300970526     C     *INZSR        BEGSR
188400970526     C*---
188500970526     C* RICEVIMENTO PARAMETRI
188600970526     C*---
188700970526     C     *ENTRY        PLIST
188800970612     C                   PARM                    KPJBA
188900001116     C                   MOVEL     KPJBU         tis720
189000020924     C*
189100020924     C* REPERISCE I DATI DELL'UTENTE
189200020924     C                   EXSR      REPDATIUTE
189300970526     C*
189400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
189500970526     C     XMSGPA        PLIST
189600970526     C                   PARM                    DSMSMS
189700970526     C                   PARM                    DSMSPG
189800970526     C                   PARM                    DSMSKE
189900970526     C                   PARM                    DSMSER
190000970526     C*
190100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
190200970526     C                   CALL      'XKEY'
190300970526     C                   PARM                    DSKEY
190400020212     C*
190500020212     C* CARICA LE TABELLE OCCORRENTI
190600020212     C                   EXSR      CARTAB
190700111216     C*
190800111216     C* Reperisco il sistema AS/400 corrente
190900111216     C                   EVAL      AS400_ok = *off
191000111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
191100111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
191200111216     C                   EVAL      AS400_ok = *on
191300111216     C                   ENDIF
191400101022     C*
191500101022     C* Apro la connessione al DB "BARTVAS"
191600111216     C                   IF        AS400_ok = *on
191700101022     C                   exsr      opnCONN
191800111216     C                   ENDIF
191900001116     C*---
192000001116     C* VARIABILI NON RIFERITE AL DATA BASE
192100001116     C*---
192200001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
192300001116     C     *LIKE         DEFINE    s20OP0        DEPOP0
192400001116     C     *LIKE         DEFINE    s20OP1        DEPOP1
192500001116     C     *LIKE         DEFINE    s20OP0        WOP0
192600001116     C     *LIKE         DEFINE    s20OP1        WOP1
192700001116     C*---
192800001116     C* VARIABILI RIFERITE AL DATA BASE
192900001116     C*---
193000001116     C     *LIKE         DEFINE    vssisv        KCEisv
193100001116     C     *LIKE         DEFINE    vsssun        KCEsun
193200001116     C     *LIKE         DEFINE    vssksu        KCEksu
193300001116     C     *LIKE         DEFINE    kuntle        KKUtle
193400001116     C     *LIKE         DEFINE    kunazp        KKUazp
193500001116     C     *LIKE         DEFINE    kuncop        KKUcop
193600020211     C     *LIKE         DEFINE    kuncof        KKUcof
193700020211     C     *LIKE         DEFINE    kundde        KKUdde
193800001116     C     *LIKE         DEFINE    tbecod        KTBcod
193900001116     C     *LIKE         DEFINE    tbeke1        KTBky1
194000010605     C     *LIKE         DEFINE    virtip        KCEtip
194100010605     C     *LIKE         DEFINE    virksc        KCEksc
194200010605     C     *LIKE         DEFINE    virdti        KCEdti
194300001116     C*---
194400001116     C* VARIABILI DI CONTROLLO
194500001116     C*---
194600001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
194700001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
194800001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
194900001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
195000010608     C                   MOVEL     '0'           ERRL01            1            *ERRORE (x TIVIR)
195100001116     C*---
195200001116     C* CHIAVI DI LETTURA
195300001116     C*---
195400001116     C* LETTURA tivss01L - COMPLETA
195500001116     C     KEYvss        KLIST
195600001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
195700001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
195800100217     C* LETTURA tivss02L - PARZIALE
195900001116     C     KE2vss        KLIST
196000001116     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
196100001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
196200100217     C* LETTURA tivss02L - COMPLETA
196300100217     C     KEYvss02_C    KLIST
196400100217     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
196500100217     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
196600100217     C                   KFLD                    KCEsun                         *STRATEGI USER NBR
196700001116     C* LETTURA tikun01L
196800001116     C     KEYkun        KLIST
196900001116     C                   KFLD                    KKUtle                         *TIPO LEGAME
197000001116     C                   KFLD                    KKUazp                         *AZIENDA PADRE
197100001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
197200001116     C* LETTURA tikun03L
197300001116     C     KE3kun        KLIST
197400001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
197500020211     C* LETTURA tikun06L
197600020211     C     KE6kun        KLIST
197700020211     C                   KFLD                    KKUcof                         *CODICE CLI FIGLIO
197800020212     C                   KFLD                    KKUtle                         *TIPO LEGAME
197900001116     C* LETTURA tntbe01l
198000001116     C     KEYtbe        KLIST
198100001116     C                   KFLD                    KTBcod                         *CODICE TABELLA
198200001116     C                   KFLD                    KTBky1                         *CHIAVE 1 TABELLA
198300010605     C* LETTURA tivir03L - COMPLETA
198400010605     C     KEYvi2        KLIST
198500010605     C                   KFLD                    KCEtip                         *TIPO FILE
198600010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
198700010605     C                   KFLD                    KCEdti                         *DATA INIZIO ABIL.
198800010605     C* LETTURA tivir03L - INCOMPLETA
198900010605     C     KEYvir        KLIST
199000010605     C                   KFLD                    KCEtip                         *TIPO FILE
199100010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
199200970526     C*
199300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
199400991104     C                   MOVEL     'TIS725R'     DSMSPG
199500991104     C                   MOVEL     'TIS725R'     MSGQUE
199600970526     C                   MOVEL     *BLANKS       MSGKEY
199700970526     C                   MOVEL     *BLANKS       DSMSMS
199800970528     C*
199900970528     C* CALCOLA LA DATA CORRENTE
200000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
200100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
200200970528     C                   Z-ADD     WN8           G08DAT
200300051013     C                   Z-ADD     WN8           DATCORINV         8 0
200400970528     C                   Z-ADD     *ZEROS        G08INV
200500970528     C                   MOVEL     '0'           G08ERR
200600970528     C                   CALL      'XSRDA8'
200700970528     C                   PARM                    WLBDA8
200800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
200900970526     C*
201000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
201100991104     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
201200991104     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
201300051013     C*
201400051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
201500051013     C                   IF        DEPOP1 = 'F06' OR
201600051013     C                             DEPOP1 = 'O03' OR
201700051013     C                             DEPOP1 = 'O07'
201800051013     C                   EVAL      VIDDDE = DATCORINV
201900060726     C                   EVAL      VIDDSC = 31122039
202000051013     C                   ENDIF
202100101022     C                   IF        DEPOP1 = 'F06' OR
202200101022     C                             DEPOP1 = 'O03'
202300101022     C                   SETOFF                                       81
202400101022     C                   ELSE
202500101022     C                   SETON                                        81
202600101022     C                   ENDIF
202700970526     C*
202800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
202900970526     C                   EXSR      IMPFUN
203000970526     C*
203100000131     C* ESEGUE SUBITO "DECODI" SE ENTRATI IN VISUALIZZAZIONE O MODIFICA
203200000915     C*    DEPOP1        IFEQ      'O05'
203300000915     C*    DEPOP1        OREQ      'O02'
203400000915     C*                  EXSR      DECODI
203500000915     C*                  ENDIF
203600000131     C*
203700970526     C                   ENDSR
203800970526     C*------------------------------------------------------------------------*
203900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
204000970526     C*------------------------------------------------------------------------*
204100970526     C     FUNGES        BEGSR
204200970526     C*
204300970526     C* PULISCO CAMPI DI LAVORO
204400970526     C                   Z-ADD     0             W                 5 0
204500970526     C*
204600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
204700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
204800970526     C     1             DO        24            I
204900970526     C                   MOVEL     FUD(I)        FUDRIG
205000970526     C*
205100970526     C* PULIZIA FUNZIONI PRECEDENTI
205200970526     C                   MOVEL     *BLANKS       FUV(I)
205300970526     C*
205400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
205500970526     C* (SALTA F24)
205600970526     C     FUDATT        IFEQ      'S'
205700970526     C     FUDFUN        ANDNE     '24'
205800970526     C                   ADD       1             W
205900970526     C                   MOVEL     FUDDES        FUV(W)
206000970526     C                   END
206100970526     C                   END                                                    DO WHILE
206200970526     C*
206300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
206400970526     C* ALTRIMENTI DISATTIVO F24
206500970526     C     1             DO        24            I
206600970526     C                   MOVEL     FUD(I)        FUDRIG
206700970526     C*
206800970526     C     FUDFUN        IFEQ      '24'
206900970526     C     W             IFGT      6
207000970526     C                   MOVEL     'S'           FUDATT
207100970526     C                   ELSE
207200970526     C                   MOVEL     *BLANKS       FUDATT
207300970526     C                   END                                                    W GT 8
207400970526     C                   MOVEL     FUDRIG        FUD(I)
207500970526     C                   END
207600970526     C                   END                                                    DO WHILE
207700970526     C*
207800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
207900970526     C     W             DIV       6             I
208000970526     C                   MVR                     Z
208100970526     C*
208200970526     C     Z             IFNE      0
208300970526     C     I             ADD       1             Z
208400970526     C                   ELSE
208500970526     C                   Z-ADD     I             Z                 5 0
208600970526     C                   END
208700970526     C*
208800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
208900970526     C                   ADD       1             FUNGRU
209000970526     C     FUNGRU        IFGT      Z
209100970526     C     FUNGRU        ORLT      1
209200970526     C                   Z-ADD     1             FUNGRU
209300970526     C                   END
209400970526     C*
209500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
209600970526     C     FUNGRU        IFEQ      1
209700970526     C                   MOVEL     FUV(1)        FUN001
209800970526     C                   MOVEL     FUV(2)        FUN002
209900970526     C                   MOVEL     FUV(3)        FUN003
210000970526     C                   MOVEL     FUV(4)        FUN004
210100970526     C                   MOVEL     FUV(5)        FUN005
210200970526     C                   MOVEL     FUV(6)        FUN006
210300970526     C                   END
210400970526     C*
210500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
210600970526     C     FUNGRU        IFEQ      2
210700970526     C                   MOVEL     FUV(7)        FUN001
210800970526     C                   MOVEL     FUV(8)        FUN002
210900970526     C                   MOVEL     FUV(9)        FUN003
211000970526     C                   MOVEL     FUV(10)       FUN004
211100970526     C                   MOVEL     FUV(11)       FUN005
211200970526     C                   MOVEL     FUV(12)       FUN006
211300970526     C                   END
211400970526     C*
211500970526     C* TERZO GRUPPO DI 6 FUNZIONI
211600970526     C     FUNGRU        IFEQ      3
211700970526     C                   MOVEL     FUV(13)       FUN001
211800970526     C                   MOVEL     FUV(14)       FUN002
211900970526     C                   MOVEL     FUV(15)       FUN003
212000970526     C                   MOVEL     FUV(16)       FUN004
212100970526     C                   MOVEL     FUV(17)       FUN005
212200970526     C                   MOVEL     FUV(18)       FUN006
212300970526     C                   END
212400970526     C*
212500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
212600970526     C     FUNGRU        IFEQ      4
212700970526     C                   MOVEL     FUV(19)       FUN001
212800970526     C                   MOVEL     FUV(20)       FUN002
212900970526     C                   MOVEL     FUV(21)       FUN003
213000970526     C                   MOVEL     FUV(22)       FUN004
213100970526     C                   MOVEL     *BLANKS       FUN005
213200970526     C                   MOVEL     *BLANKS       FUN006
213300970526     C                   END
213400970526     C*
213500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
213600970526     C                   MOVEL     *BLANKS       FUN007
213700970526     C                   MOVEL     *BLANKS       FUN008
213800970526     C*
213900970526     C     1             DO        24            I
214000970526     C                   MOVEL     FUD(I)        FUDRIG
214100970526     C     FUDFUN        IFEQ      '24'
214200970526     C     FUDATT        ANDEQ     'S'
214300970526     C                   MOVEL     FUDDES        FUN008
214400970526     C                   END
214500970526     C                   END                                                    DO
214600970526     C*
214700970526     C                   ENDSR
214800970526     C*------------------------------------------------------------------------*
214900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
215000970526     C*------------------------------------------------------------------------*
215100970526     C     FUNCON        BEGSR
215200970526     C*
215300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
215400970526     C                   EXSR      FUNVAR
215500970526     C*
215600970526     C* PULISCO CAMPI DI LAVORO
215700970526     C                   Z-ADD     0             I                 5 0
215800970526     C                   Z-ADD     0             W                 5 0
215900970526     C*
216000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
216100970526     C     FUNKEY        IFNE      *BLANKS
216200970526     C     1             DO        24            I
216300970526     C                   MOVEL     FUD(I)        FUDRIG
216400970526     C*
216500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
216600970526     C     FUNKEY        IFEQ      FUDFUN
216700970526     C     FUDATT        ANDEQ     'S'
216800970526     C                   Z-ADD     I             W
216900970526     C                   END
217000970526     C                   END                                                    DO WHILE
217100970526     C*
217200970526     C* FUNZIONE NON ATTIVA (ERRORE)
217300970526     C     W             IFEQ      0
217400970526     C*
217500970526     C* EMISSIONE MESSAGGIO DI ERRORE
217600970526     C                   MOVEL     MSG(1)        DSMSMS
217700970526     C*
217800970526     C                   END                                                    W=0
217900970526     C                   END                                                    FUNKEY NE *BLANKS
218000970526     C*
218100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
218200970526     C     W             IFNE      0
218300970526     C     FUNKEY        OREQ      *BLANKS
218400970526     C     FUNKEY        IFEQ      *BLANKS
218500970526     C                   EXSR      ENTGES
218600970526     C                   END
218700970526     C     FUNKEY        IFEQ      '03'
218800970526     C                   EXSR      F03GES
218900970526     C                   END
219000970526     C     FUNKEY        IFEQ      '06'
219100970526     C                   EXSR      F06GES
219200970526     C                   END
219300970526     C     FUNKEY        IFEQ      '12'
219400970526     C                   EXSR      F12GES
219500970526     C                   END
219600100914     C     FUNKEY        IFEQ      '18'
219700100914     C                   EXSR      F18GES
219800100914     C                   END
219900970526     C     FUNKEY        IFEQ      '24'
220000970526     C                   EXSR      F24GES
220100970526     C                   END
220200970526     C                   END
220300970526     C*
220400970526     C                   ENDSR
220500970526     C*------------------------------------------------------------------------*
220600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
220700970526     C*------------------------------------------------------------------------*
220800970526     C     FUNVAR        BEGSR
220900970526     C*
221000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
221100970526     C*
221200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
221300970526     C     KEY           IFEQ      ENTER
221400970526     C                   MOVEL     *BLANKS       FUNKEY
221500970526     C                   END
221600970526     C     KEY           IFEQ      F1
221700970526     C                   MOVEL     '01'          FUNKEY
221800970526     C                   END
221900970526     C     KEY           IFEQ      F2
222000970526     C                   MOVEL     '02'          FUNKEY
222100970526     C                   END
222200970526     C     KEY           IFEQ      F3
222300970526     C                   MOVEL     '03'          FUNKEY
222400970526     C                   END
222500970526     C     KEY           IFEQ      F4
222600970526     C                   MOVEL     '04'          FUNKEY
222700970526     C                   END
222800970526     C     KEY           IFEQ      F5
222900970526     C                   MOVEL     '05'          FUNKEY
223000970526     C                   END
223100970526     C     KEY           IFEQ      F6
223200970526     C                   MOVEL     '06'          FUNKEY
223300970526     C                   END
223400970526     C     KEY           IFEQ      F7
223500970526     C                   MOVEL     '07'          FUNKEY
223600970526     C                   END
223700970526     C     KEY           IFEQ      F8
223800970526     C                   MOVEL     '08'          FUNKEY
223900970526     C                   END
224000970526     C     KEY           IFEQ      F9
224100970526     C                   MOVEL     '09'          FUNKEY
224200970526     C                   END
224300970526     C     KEY           IFEQ      F10
224400970526     C                   MOVEL     '10'          FUNKEY
224500970526     C                   END
224600970526     C     KEY           IFEQ      F11
224700970526     C                   MOVEL     '11'          FUNKEY
224800970526     C                   END
224900970526     C     KEY           IFEQ      F12
225000970526     C                   MOVEL     '12'          FUNKEY
225100970526     C                   END
225200970526     C     KEY           IFEQ      F13
225300970526     C                   MOVEL     '13'          FUNKEY
225400970526     C                   END
225500970526     C     KEY           IFEQ      F14
225600970526     C                   MOVEL     '14'          FUNKEY
225700970526     C                   END
225800970526     C     KEY           IFEQ      F15
225900970526     C                   MOVEL     '15'          FUNKEY
226000970526     C                   END
226100970526     C     KEY           IFEQ      F16
226200970526     C                   MOVEL     '16'          FUNKEY
226300970526     C                   END
226400970526     C     KEY           IFEQ      F17
226500970526     C                   MOVEL     '17'          FUNKEY
226600970526     C                   END
226700970526     C     KEY           IFEQ      F18
226800970526     C                   MOVEL     '18'          FUNKEY
226900970526     C                   END
227000970526     C     KEY           IFEQ      F19
227100970526     C                   MOVEL     '19'          FUNKEY
227200970526     C                   END
227300970526     C     KEY           IFEQ      F20
227400970526     C                   MOVEL     '20'          FUNKEY
227500970526     C                   END
227600970526     C     KEY           IFEQ      F21
227700970526     C                   MOVEL     '21'          FUNKEY
227800970526     C                   END
227900970526     C     KEY           IFEQ      F22
228000970526     C                   MOVEL     '22'          FUNKEY
228100970526     C                   END
228200970526     C     KEY           IFEQ      F23
228300970526     C                   MOVEL     '23'          FUNKEY
228400970526     C                   END
228500970526     C     KEY           IFEQ      F24
228600970526     C                   MOVEL     '24'          FUNKEY
228700970526     C                   END
228800970526     C     KEY           IFEQ      ROLLUP
228900970526     C                   MOVEL     'RU'          FUNKEY
229000970526     C                   END
229100970526     C     KEY           IFEQ      HELP
229200970526     C                   MOVEL     'HP'          FUNKEY
229300970526     C                   END
229400970526     C                   ENDSR
229500970526     C*------------------------------------------------------------------------*
229600970526     C* NOTE COSTRUZIONE SCHIERE
229700970526     C*------------------------------------------------------------------------*
229800970526     C*
229900970526     C* -MSG-MESSAGGI
230000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
230100970526     C* (NON TOCCARE MSG,1 E MSG,2)
230200970526     C*
230300970526     C* -CMD-COMANDI
230400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
230500970526     C*
230600970526     C* -OPD-OPZIONI
230700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
230800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
230900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
231000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
231100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
231200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
231300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
231400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
231500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
231600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
231700970526     C*
231800970526     C* -FUD-FUNZIONI
231900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
232000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
232100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
232200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
232300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
232400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
232500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
232600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
232700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
232800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
232900970526     O*------------------------------------------------------------------------*
233000970526** DEC - DECODIFICHE
233100970526    MODIFICA                                                      1
233200970526  ANNULLAMENTO                                                    2
233300970526VISUALIZZAZIONE                                                   3
233400970526   RIPRISTINO                                                     4
233500970526  INSERIMENTO                                                     5
233600991105     COPIA                                                        6
233700000215** DES - DESCRIZIONI
233800000215ATTENZIONE: non esistono figli legati                             1
233900000215ATTENZIONE: esistono figli legati                                 2
234000020211                                                                  3
234100000215                                                                  4
234200000215                                                                  5
234300970526** MSG - MESSAGGI
234400991104TIS725R- Tasto funzionale non gestito                                          1
234500991104TIS725R- Inserimento effettuato                                                2
234600991104TIS725R- Modifica effettuata                                                   3
234700991104TIS725R- Annullamento effettuato                                               4
234800991104TIS725R- Ripristino effettuato                                                 5
234900991104TIS725R- Richiesta funzione per record inesistente                             6
235000991104TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
235100991104TIS725R- Richiesta funzione per record esistente                               8
235200991104TIS725R- Codice tipo servizio non presente in tabella                          9
235300000322TIS725R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
235400000317TIS725R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
235500091117TIS725R- Tipo legame inesistente o non utilizzabile                            12
235600991104TIS725R- Codice padre errato                                                   13
235700991104TIS725R- Data errata                                                           14
235800991104TIS725R- La data scadenza deve essere maggiore della decorrenza                15
235900991104TIS725R- Il record non può scadere in un giorno già trascorso                  16
236000991104TIS725R- Il record immesso si sovrappone ad uno già in decorrenza              17
236100991104TIS725R- Codice figlio errato                                                  18
236200000705TIS725R- Codice padre obbligatorio                                             19
236300991104TIS725R- Il codice figlio non può essere uguale al codice padre                20
236400000719TIS725R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
236500000807TIS725R- Tipo legame non autorizzato.                                          22
236600000807TIS725R- Filiale non abilitata alla gestione dei clienti scelti.               23
236700000913TIS725R- Servizio DL già presente nel periodo per il cliente scelto.           24
236800010605TIS725R- Nel periodo esistono già abilitazioni per il cliente al file TO.      25
236900010605TIS725R- Abilitazione per il cliente al file TO già esistente.                 26
237000020211TIS725R- Cliente già presente nei legami come figlio.                          27
237100020214TIS725R- Per il servizio indicato non è consentito specificare un legame.      28
237200020802TIS725R- Il codice Strategi è incompatibile con il servizio richiesto.         29
237300090804TIS725R- Campo valido solamente per servizi 'TT'                               30
237400100217TIS725R- E' necessaria una identica abilitazione al servizio 'TT'              31
237500100913TIS725R- Password necessaria x attivazione servizio richiesto.                 32
237600100913TIS725R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    33
237700100913TIS725R- Lunghezza minima password: 8 caratteri                                34
237800100914TIS725R- Tasto funzione non consentito per servizi diversi da 'IE'             35
237900101027TIS725R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       36
238000120327TIS725R- Cliente annullato o bloccato, impossibile abilitare EasyWeb           37
238100101027TIS725R-                                                                       38
238200101027TIS725R-                                                                       39
238300101027TIS725R-                                                                       40
238400970526** FUD - FUNZIONI
238500970526                   -        (S/N)=N                        funz.01¬
238600970526                   -        (S/N)=N                        funz.02¬
238700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
238800970526                   -        (S/N)=N                        funz.04¬
238900970526                   -        (S/N)=N                        funz.05¬
239000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
239100970527                   -        (S/N)=N                        funz.07¬
239200970527                   -        (S/N)=N                        funz.08¬
239300970527                   -        (S/N)=N                        funz.09¬
239400970527                   -        (S/N)=N                        funz.10¬
239500970526                   -        (S/N)=N                        funz.11¬
239600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
239700970526                   -        (S/N)=N                        funz.13¬
239800970526                   -        (S/N)=N                        funz.14¬
239900970526                   -        (S/N)=N                        funz.15¬
240000970526                   -        (S/N)=N                        funz.16¬
240100970526                   -        (S/N)=N                        funz.17¬
240200100914F18=Conf.EasySped  -        (S/N)=S                        funz.18¬
240300970526                   -        (S/N)=N                        funz.19¬
240400970526                   -        (S/N)=N                        funz.20¬
240500970526                   -        (S/N)=N                        funz.21¬
240600970526                   -        (S/N)=N                        funz.22¬
240700970526                   -        (S/N)=N                        funz.23¬
240800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
