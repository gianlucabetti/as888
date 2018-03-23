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
009500170915     D tis725DS3     E DS
009600090804     D dvssdta       E DS
009700100217     D tivssds       E DS                  extname(tivss00f)
009800100914     D tis725ds21    E DS                  prefix(S725_)
009900170111     D tis732ds      E DS
010000970526     D*------------
010100970526     D* ARCHITETTURA
010200970526     D*------------
010300970526     D KPJBA         E DS
010400100406     D*------------
010500100406     D* DS DEFINIZIONE TABELLA '3EW'
010600100406     D*------------
010700100406     D D3EW          E DS                  qualified
010800000807     D*------------
010900000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
011000000807     D ksc             s              7  0 dim(30)
011100000807     D ksa             s              4    dim(30)
011200020212     D isvkun          s              2    dim(100)
011300050118     D isvtt           s              2    dim(100)
011400100913     D isvftp          s              2    dim(100)
011500100217     D isvKEYS         s              2    dim(100)
011600100217     D isvCTRL         s              4    dim(100)
011700101006     D isvPGMAFT       s              3    dim(100)
011800000807     D*------------------
011900000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
012000000807     D*------------------
012100000807     DDSXC           e DS                  extname(XCLIRDS)
012200000807     D*---
012300000807     D                 DS
012400000807     D dsksn                   1      4p 0
012500000807     D dsksa                   1      4
012600000807     DBS69DS         e ds                  extname(TIBS69DS)
012700000807     DACODS          e ds                  extname(cnaco00f)
012800000807     DINDDS          e ds                  extname(cnind00f)
012900000807     DCLPDS          e ds                  extname(cnclp00f)
013000000807     DCLSDS          e ds                  extname(fncls00f)
013100000807     D*---
013200000807     D* VARIABILI NON RIFERITE AL DATABASE
013300000807     D*---
013400000807     D $esikun         S              1                                         *Esistenza KUN
013500000807     D chk             S              1    inz('1')
013600000807     D giro            S              1  0 inz(*zeros)
013700000807     D tipprf          s              3    inz(*blanks)
013800040422     D cmd             s            500
013900040422     D api             s              1    inz('''')
014000090611     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
014100091027     D sktle           S              2    DIM(20)
014200091027     D jtle            S              5  0                                      *indici
014300100914     D OPERAZ          S              3A   inz
014400100914     D savKPJBU        S                   like(KPJBU)
014500100914     D ds_param        S           1024    inz
014600100913     D*
014700100913     D                 ds
014800100913     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
014900100913     D                                     456789abcdefghijklmnopqrstuvwxyz')
015000100913     D  valAry                        1A   DIM(63)
015100100913     D                                     OVERLAY(val)
015200000807     D*------------
015300000807     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
015400000807     D*------------
015500000807     D                 DS
015600000807     D  FILCLI                 5      7  0
015700000807     D  STRATECLI              1     11
015800020924     D*-------------------
015900020924     D* DS REPERIMENTO DATI UTENTE
016000020924     D*-------------------
016100020924     D TIBS34DS      E DS                                                       *Profili utente
016200020924     D DDATIUTE      E DS                                                       *Dati utente
016300020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016400111216     D*------------------
016500111216     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
016600111216     D*------------------
016700111216     D currSysNeta     s              8A   inz('*NULL')
016800111216     D AS400_ok        s              1A
016900111216     D  PiOutSysNeta   S              8A
017000090610     D*-------------------
017100090610     D* DEFINIZIONI PROCEDURE ESTERNE
017200090610     D*-------------------
017300090610     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
017400090610     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
017500101022     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
017600101022     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
017700100406     D/COPY GAITRASRC/SRCPROTOPR,UB§3EWUPD
017800110419     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
017900110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
018000111216     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
018100970526     C*------------------------------------------------------------------------*
018200970526     C* MAIN LINES
018300970526     C*------------------------------------------------------------------------*
018400970526     C*
018500970526     C* NON CI SONO ERRORI IN ENTRATA
018600000915IF  1C*    ERRL00        IFEQ      '0'
018700970526     C*
018800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
018900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
019000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
019100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
019200970526     C*
019300970526     C* EMETTE FORMATO VIDEO
019400970526     C                   EXSR      BUTFMR
019500970526     C*
019600970526     C* GESTIONE TASTI FUNZIONALI
019700970526     C                   EXSR      FUNCON
019800970526E   2C                   ENDDO
019900970526     C*
020000970526     C* CI SONO ERRORI IN ENTRATA
020100000915X   1C*                  ELSE
020200000915     C*                  MOVEL     '1'           s20err
020300000915     C*                  MOVEL     DSMSMS        s20msg
020400000915E   1C*                  ENDIF
020500001116     C*
020600970526     C* FINE PGM
020700001116     C                   EXSR      FINSR
020800001116     C*
020900100914     C                   MOVEL     tis720        KPJBU
021000101022     C*
021100101022     C* Chiudo la connessione
021200111216     C                   IF        AS400_ok = *on
021300101022     C                   exsr      clsCONN
021400111216     C                   ENDIF
021500101022     C*
021600970526     C                   SETON                                        LR
021700100913     C*------------------------------------------------------------------------*
021800100913     C* RESETIN - RESET INDIRCATORI
021900100913     C*------------------------------------------------------------------------*
022000100913     C     RESETIN       BEGSR
022100100913     C*
022200100913     C* REIMPOSTA INDICATORI DI ERRORE
022300100913     C                   SETOFF                                         5859
022400100913     C                   SETOFF                                       606162
022500100913     C                   SETOFF                                       636465
022600100913     C                   SETOFF                                       666768
022700100913     C                   SETOFF                                       697071
022800100913     C                   SETOFF                                       727374
022900100913     C                   SETOFF                                       757677
023000100913     C                   SETOFF                                       787980
023100100913     C*
023200100913     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
023500970526     C*------------------------------------------------------------------------*
023600970526     C     BUTFMR        BEGSR
023700970526     C*
023800970526     C* EMETTE LA TESTATA
023900991104     C                   WRITE     S725D01
024000970526     C*
024100970526     C* SPEDISCE EVENTUALE MESSAGGIO
024200970526     C                   EXSR      SNDMSG
024300970526     C*
024400970526     C* EMETTE LE FUNZIONI
024500991104     C                   WRITE     S725D02
024600970526     C*
024700970526     C* EMETTE IL VIDEO
024800991104     C                   EXFMT     S725D03
024900970526     C*
025000970526     C* REIMPOSTA FLAG DI ERRORE
025100970526     C                   MOVEL     '0'           ERRL00
025200100913     C*
025300100913     C* RESETTA INDICATORI DI CONTROLLO
025400100913     C                   EXSR      RESETIN
025500970526     C*
025600970526     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
025900970526     C*------------------------------------------------------------------------*
026000970526     C     SNDMSG        BEGSR
026100970526     C*
026200970526     C* PULISCE IL SFILE MESSAGGI
026300970526     C                   MOVEL     '1'           *IN40
026400991104     C                   WRITE     S725DMC
026500970526     C                   MOVEL     '0'           *IN40
026600970526     C*
026700970526     C* VISUALIZZA IL MESSAGGIO
026800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
026900970526     C                   CALL      'XMSG'        XMSGPA
027000970526IF  1C     DSMSER        IFEQ      *ZEROS
027100970526     C                   MOVEL     DSMSKE        MSGKEY
027200970526E   1C                   ENDIF
027300970526     C                   Z-ADD     1             MSGULT
027400991104     C                   WRITE     S725DMS
027500991104     C                   WRITE     S725DMC
027600970526     C*
027700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
027800970526     C                   MOVEL     *BLANKS       DSMSMS
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* ENTGES - GESTIONE ENTER -> INVIO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     ENTGES        BEGSR
028500970526     C*
028600970526     C* CONTROLLA I CAMPI IMMESSI
028700970526     C                   EXSR      DECODI
028800970526     C*
028900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
029000970526IF  1C     DEPOP1        IFEQ      'O05'
029100970526     C                   MOVEL     '1'           FINL00
029200970526E   1C                   ENDIF
029300970526     C*
029400970526     C                   ENDSR
029500970526     C*------------------------------------------------------------------------*
029600970526     C* F03GES - GESTIONE F03 -> FINE
029700970526     C*------------------------------------------------------------------------*
029800970526     C     F03GES        BEGSR
029900970526     C*
030000970526     C* IMPOSTA "PREMUTO F03"
030100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
030200991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
030300970526     C*
030400970526     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
030700970526     C*------------------------------------------------------------------------*
030800970526     C     F06GES        BEGSR
030900970526     C*
031000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
031100970526IF  1C     DEPOP1        IFEQ      'F06'
031200100914     C                   EVAL      OPERAZ = 'INS'
031300970526     C                   EXSR      INSERI
031400970526E   1C                   ENDIF
031500970526IF  1C     DEPOP1        IFEQ      'O02'
031600100914     C                   EVAL      OPERAZ = 'UPD'
031700970526     C                   EXSR      MODIFI
031800090610     C                   EXSR      CHKSGI
031900970526E   1C                   ENDIF
032000991014IF  1C     DEPOP1        IFEQ      'O03'
032100100914     C                   EVAL      OPERAZ = 'INS'
032200000719     C                   EXSR      COPIA
032300991014E   1C                   ENDIF
032400021024IF  1C     DEPOP1        IFEQ      'O04'
032500100914     C                   EVAL      OPERAZ = 'DEL'
032600021024     C                   EXSR      ANNULL
032700090610     C                   EXSR      CHKSGI
032800021024E   1C                   ENDIF
032900970526IF  1C     DEPOP1        IFEQ      'O07'
033000100914     C                   EVAL      OPERAZ = 'UPD'
033100970526     C                   EXSR      RIPRIS
033200090610     C                   EXSR      CHKSGI
033300970526E   1C                   ENDIF
033400101012     C*
033500101012     C                   ENDSR
033600101012     C*------------------------------------------------------------------------*
033700101012     C* EXEPOST - GESTIONE "POST AGGIORNAMNETO"
033800101012     C*------------------------------------------------------------------------*
033900101012     C     EXEPOST       BEGSR
034000101012     C*
034100100217     C*
034200110210     C* Se no errore => gestione eventuale *PGM POST-AGGIORNAMENTO (solo x "INSERIMENTI")
034300110214 xxx C***                if        ERRL00 <> '1' and OPERAZ = 'INS'
034400110214 xxx C                   if        ERRL00 <> '1'
034500101005     C                   seton                                        33
034600101005     C*
034700101006     C                   movel     *blanks       wPGMAFTER         3
034800100217     C                   z-add     1             jISV              3 0
034900100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
035000100217     C                   if        *in22 = *on
035100100217     C                   if        isvPGMAFT(jISV) <> *blanks
035200100217     C                   eval      wPGMAFTER = isvPGMAFT(jISV)
035300100914     C                   eval      savKPJBU = KPJBU
035400100914     C*
035500101004     C                   select
035600110210     C* - 002
035700110210     C                   when      wPGMAFTER = '002'
035800100914     C                   clear                   TIS725DS21
035900100914     C                   select
036000110210     C                   when      FUNKEY = '06'
036100101022 xxx C***                setoff                                       33
036200101005     C                   eval      s725_§21_FILE   = 'VSS'
036300100914     C                   when      FUNKEY = '18'
036400110210 xxx C***                setoff                                       33
036500100914     C                   eval      s725_§21_FILE   = 'ESY'
036600100914     C                   endsl
036700100914     C                   eval      s725_§21_OPERAZ = OPERAZ
036800100914     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
036900100914     C                   eval      s725_§21_DESCR  = VDDKSU
037000100914     C                   eval      s725_§21_PASSW  = VIDPWDFTP
037100100914     C                   eval      ds_param = TIS725DS21
037200101006     C* - 003
037300110210     C                   when      wPGMAFTER = '003'
037400101005     C                   clear                   TIS725DS21
037500101022 xxx C***                setoff                                       33
037600101005     C                   eval      s725_§21_FILE   = 'VSS'
037700101005     C                   eval      s725_§21_OPERAZ = OPERAZ
037800101005     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
037900101005     C                   eval      s725_§21_DESCR  = VDDKSU
038000101005     C                   eval      s725_§21_PASSW  = VIDPWDFTP
038100101005     C                   eval      ds_param = TIS725DS21
038200170111     C* - 004
038300170111     C                   when      wPGMAFTER = '004'
038400170111     C                   clear                   TIS732DS
038500170111 xxx C***                setoff                                       33
038600170112     C                   eval      S32OP0 = DEPOP0
038700170112     C                   eval      S32OP1 = DEPOP1
038800170112     C* Gestisco Opzione di COPIA come un INSERIMENTO
038900170112     C                   if        S32OP1 = 'O03'
039000170112     C                   eval      S32OP1 = 'F06'
039100170112     C                   endif
039200170111     C                   eval      S32SUN = VIDSUN
039300170111     C                   eval      S32ISV = VIDISV
039400170111     C                   eval      S32TIP = 'VAS'
039500170111     C                   eval      S32KSU = VIDKSU
039600170111     C                   eval      ds_param = TIS732DS
039700101004     C*
039800101004     C                   endsl
039900100914     C*
040000101005     C   33              call      'TIS725R1'
040100100914     C                   parm                    KPJBA
040200100217     C                   parm                    wPGMAFTER
040300100217     C                   parm                    tivssds
040400100914     C                   parm                    ds_param
040500100914     C*
040600101006     C                   if        wPGMAFTER = '002' OR
040700101006     C                             wPGMAFTER = '003'
040800100914     C                   eval      TIS725DS21 = ds_param
040900100914     C                   if        s725_§21_ERRFLG <> *blanks
041000100914     C                   eval      s20MSG = s725_§21_ERRMSG
041100100914     C                   eval      DSMSMS = s725_§21_ERRMSG
041200100914     C                   endif
041300100914     C                   if        s725_§21_ERRFLG <> *blanks
041400100914     C                   MOVEL     '1'           ERRL00
041500100914     C                   endif
041600100914     C                   endif
041700100914     C*
041800100914     C                   eval      KPJBU = savKPJBU
041900100217     C                   endif
042000100217     C                   endif
042100101005     C*
042200101005     C                   endif
042300970526     C*
042400970526     C                   ENDSR
042500170915     C*------------------------------------------------------------------------*
042600170915     C* F08GES - GESTIONE F08 -> Anagrafiche varie
042700170915     C*------------------------------------------------------------------------*
042800170915     C     F08GES        BEGSR
042900170915     C*
043000170915     C* valorizza DS TIS725DS3
043100170915     C                   EVAL      S253OP0 =     S20OP0
043200170915     C                   EVAL      S253OP1 =     S20OP1
043300170915     C                   EVAL      S253KSU = VIDKSU
043400170915     C                   EVAL      KPJBU = TIS725DS3
043500170915     C                   CALL      'TIS725R3'
043600170915     C                   PARM                    KPJBA
043700170915     C*
043800170915     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* F12GES - GESTIONE F12 -> RITORNO
044100970526     C*------------------------------------------------------------------------*
044200970526     C     F12GES        BEGSR
044300970526     C*
044400970526     C* IMPOSTA "PREMUTO F12"
044500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
044600991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
044700970526     C*
044800970526     C                   ENDSR
044900100914     C*------------------------------------------------------------------------*
045000100914     C* F18GES - GESTIONE F18 -> AGGIORNAMENTO EASYSPED
045100100914     C*------------------------------------------------------------------------*
045200100914     C     F18GES        BEGSR
045300100914     C*
045400100914     C                   EXSR      F06GES
045500110419     C*
045600110419     C* Forzo l'elaborazione lato server
045700110419     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
045800110419     C                                           '0ADMIN':'#U')
045900100914     C*
046000100914     C                   ENDSR
046100970526     C*------------------------------------------------------------------------*
046200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
046300970526     C*------------------------------------------------------------------------*
046400970526     C     F24GES        BEGSR
046500970526     C*
046600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
046700970526     C*
046800970526     C                   ENDSR
046900970526     C*------------------------------------------------------------------------*
047000010605     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
047100970526     C*------------------------------------------------------------------------*
047200970526     C     INSERI        BEGSR
047300970526     C*
047400970526     C* CONTROLLA I CAMPI IMMESSI
047500970526     C                   EXSR      DECODI
047600000731     C                   IF        chk = '1' and giro = 1
047700000719     C                   MOVEL     '1'           ERRL00
047800000719     C                   ENDIF
047900970526     C*
048000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
048100010605     C     ERRL00        IFEQ      '0'                                          *ANCORA NO ERRORI
048200000719     C                   MOVE      VIDsun        vsssun
048300010605     C                   MOVE      VIDisv        vssisv
048400991104     C                   EXSR      IMPvss                                       *ALTRI CAMPI
048500991104     C                   Z-ADD     *ZEROS        vssDTR                         *DATA TRASMISSIONE
048600991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
048700991104     C                   WRITE     tivss000
048800000719     C                   Z-ADD     *zeros        giro
048900100914     C                   MOVEL     MSG(2)        s20MSG                         *MESSAGGIO
049000100914     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
049100010605     C*
049200010605     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
049300010608     C                   IF        ERRL01 = '0'  and
049400010608     C                             VIDisv = 'TT'
049500010626 ORM C*                  EXSR      IMPvir
049600010626 ORM C*                  WRITE     tivir000
049700010605     C                   ENDIF
049800040422     C*
049900040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
050000040422     C                   EXSR      EXESGI
050100101012     C*
050200101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
050300101012     C                   EXSR      EXEPOST
050400970602     C*
050500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
050600970602     C                   EXSR      AZZVID
050700010605     C                   ENDIF
050800970526     C*
050900970526     C                   ENDSR
051000970526     C*------------------------------------------------------------------------*
051100970526     C* MODIFI - OPERAZIONI DI MODIFICA
051200970526     C*------------------------------------------------------------------------*
051300970526     C     MODIFI        BEGSR
051400970526     C*
051500970526     C* CONTROLLA I CAMPI IMMESSI
051600970526     C                   EXSR      DECODI
051700000719     C*--------------------
051800000719     C* codice strategi
051900000719     C*--------------------
052000000731     C                   IF        chk = '1' and giro = 1
052100000719     C                   MOVEL     '1'           ERRL00
052200000719     C                   ENDIF
052300970526     C*
052400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
052500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052600991104     C                   EXSR      LETvss
052700970526     C*
052800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
052900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
053000991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
053100110214     C*
053200110214     C                   SETOFF                                       41
053300110214     C                   IF        DDEVID > datcor or
053400110214     C                             DSCVID < datcor
053500110214     C                   SETON                                        41
053600110214     C                   ENDIF
053700110214     C*
053800110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
053900110214     C   41              EXSR      EXEPOST
054000110214     C*
054100991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
054200991104     C                   UPDATE    tivss000
054300101008     C*
054400101008     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
054500101008     C                   EXSR      EXESGI
054600101027     C                   EVAL      s20MSG = MSG(3)
054700101027     C                   EVAL      DSMSMS = MSG(3)
054800101022     C*
054900101022     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
055000101022     C                   IF        *IN80
055100111216     C                             and AS400_ok = *on
055200101022      /FREE
055300101022
055400101022         if UBBVSQL01_UpdUnqUtente(
055500101022                         pInConn
055600101022                       : %subst(VIDKSU:2:7)
055700101022                       : VIDPWDFTP) = 0;
055800101027         else;
055900101027              s20MSG = MSG(36);
056000101027              DSMSMS = MSG(36);
056100101022         endif;
056200101022
056300101022      /END-FREE
056400101022     C
056500101022     C                   ENDIF
056600101008     C*
056700000719     C                   Z-ADD     *zeros        giro
056800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
056900110214     C*
057000110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
057100110214     C  N41              EXSR      EXEPOST
057200110214     C*
057300970526E   2C                   ENDIF
057400970526E   1C                   ENDIF
057500970526     C*
057600970526     C                   ENDSR
057700991014     C*------------------------------------------------------------------------*
057800000719     C* COPIA  - OPERAZIONI DI COPIA
057900991014     C*------------------------------------------------------------------------*
058000000719     C     COPIA         BEGSR
058100991014     C*
058200991014     C* CONTROLLA I CAMPI IMMESSI
058300991014     C                   EXSR      DECODI
058400000719     C*--------------------
058500000719     C* codice strategi
058600000719     C*--------------------
058700000731     C                   IF        chk = '1' and giro = 1
058800000719     C                   MOVEL     '1'           ERRL00
058900000719     C                   ENDIF
059000991014     C*
059100991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
059200991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
059300991105     C                   MOVEL     VIDsun        vsssun                         *CHIAVE
059400991104     C                   MOVEL     vidisv        vssisv
059500991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
059600991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
059700001116     C                   WRITE     tivss000
059800000719     C                   Z-ADD     *zeros        giro
059900991104     C                   MOVEL     MSG(3)        s20MSG
060000000719     C                   MOVEL     MSG(3)        DSMSMS
060100991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
060200010612     C*
060300010612     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
060400010612     C                   IF        ERRL01 = '0'  and
060500010612     C                             VIDisv = 'TT'
060600010626 ORM C*                  EXSR      IMPvir
060700010626 ORM C*                  WRITE     tivir000
060800010612     C                   ENDIF
060900040422     C*
061000040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
061100040422     C                   EXSR      EXESGI
061200101012     C*
061300101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
061400101012     C                   EXSR      EXEPOST
061500991014E   2C                   ENDIF
061600991014     C*
061700991014     C                   ENDSR
061800970526     C*------------------------------------------------------------------------*
061900970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
062000970526     C*------------------------------------------------------------------------*
062100991104     C*    ANNULL        BEGSR
062200970526     C*
062300970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
062400991104     C*                  EXSR      LETvss
062500970527     C*
062600970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
062700991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
062800991104IF  2C*    vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
062900991104IF  3C*    vssDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
063000991104     C*                  MOVEL     'A'           vssvat                         *FLAG ANNULLAMENTO
063100991104     C*                  MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
063200991104     C*                  UPDATE    tivss000                                     *ANNULLA
063300991104X   3C*                  ELSE                                                   *MAI TRASMESSO
063400991104     C*                  DELETE    tivss000                                     *CANCELLA
063500991104E   3C*                  ENDIF
063600991104E   2C*                  ENDIF
063700991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
063800991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
063900991104E   1C*                  ENDIF
064000970526     C*
064100991104     C*                  ENDSR
064200021024     C*------------------------------------------------------------------------*
064300021024     C* ANNULL - OPERAZIONI DI FAR SCADERE
064400021024     C*------------------------------------------------------------------------*
064500021024     C     ANNULL        BEGSR
064600021024     C*
064700021024     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
064800021024     C                   EXSR      LETvss
064900021024     C*
065000021024     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
065100021024IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
065200021024IF  2C     vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
065300090611     C* Faccio scadere a oggi - 1 giorno
065400090611     C     *ISO          MOVE      datcor        wDataISO
065500090611     C     wDataISO      SUBDUR    1:*D          wDataISO
065600110214     C     *ISO          MOVE      wDataISO      DSCVID
065700090611     C     *ISO          MOVE      wDataISO      vssdsc
065800090611     C*
065900021024     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
066000110214     C*
066100110214     C                   SETOFF                                       41
066200110214     C                   IF        DDEVID > datcor or
066300110214     C                             DSCVID < datcor
066400110214     C                   SETON                                        41
066500110214     C                   ENDIF
066600110214     C*
066700110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
066800110214     C   41              EXSR      EXEPOST
066900110214     C*
067000021024     C                   UPDATE    tivss000                                     *ANNULLA
067100110214     C*
067200110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
067300110214     C  N41              EXSR      EXEPOST
067400110214     C*
067500021024E   2C                   ENDIF
067600021024     C                   MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
067700021024     C                   MOVEL     '1'           FINL00                         *FINE PGM
067800021024E   1C                   ENDIF
067900021024     C*
068000021024     C                   ENDSR
068100970526     C*------------------------------------------------------------------------*
068200970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
068300970526     C*------------------------------------------------------------------------*
068400970526     C     RIPRIS        BEGSR
068500970526     C*
068600970526     C* CONTROLLA I CAMPI IMMESSI
068700970526     C                   EXSR      DECODI
068800000719     C*--------------------
068900000719     C* codice strategi
069000000719     C*--------------------
069100000731     C                   IF        chk = '1' and giro = 1
069200000719     C                   MOVEL     '1'           ERRL00
069300000719     C                   ENDIF
069400970526     C*
069500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
069600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
069700991104     C                   EXSR      LETvss
069800970526     C*
069900970526     C* IMPOSTA IL RECORD DA SCRIVERE
070000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
070100991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
070200991104     C                   UPDATE    tivss000                                     *ANNULLA
070300000719     C                   Z-ADD     *zeros        giro
070400970526E   2C                   ENDIF
070500991104     C                   MOVEL     MSG(5)        s20MSG                         *MESSAGGIO
070600000719     C                   MOVEL     MSG(5)        DSMSMS                         *MESSAGGIO
070700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
070800101012     C*
070900101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
071000101012     C                   EXSR      EXEPOST
071100970526E   1C                   ENDIF
071200970526     C*
071300970526     C                   ENDSR
071400970526     C*------------------------------------------------------------------------*
071500991104     C* LETvss - LEGGE IL RECORD PERL'AGGIORNAMENTO
071600970526     C*------------------------------------------------------------------------*
071700991104     C     LETvss        BEGSR
071800970526     C*
071900100322     C                   MOVE      vidsun        KCEsun
072000100924     C***                MOVE      vidisv        KCEisv
072100100924     C                   MOVE      vssisv        KCEisv
072200991104     C     KEYvss        CHAIN     tivss01L                           9998
072300970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
072400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
072500991104     C                   MOVEL     MSG(7)        s20MSG                         *MESSAGGIO
072600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
072700970526E   1C                   ENDIF
072800970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
072900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
073000991104     C                   MOVEL     MSG(6)        s20msg                         *MESSAGGIO
073100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
073200970526E   1C                   ENDIF
073300970526     C*
073400970526     C                   ENDSR
073500970526     C*------------------------------------------------------------------------*
073600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
073700970526     C*------------------------------------------------------------------------*
073800970526     C     DECODI        BEGSR
073900000719     C*
074000000719     C                   MOVEL     '0'           ERRL00
074100000131     C*
074200991104     C*--------------------
074300991104     C* tipo servizio
074400991104     C*--------------------
074500991104IF  1C     ERRL00        IFEQ      '0'
074600991104IF  2C     VIDisv        IFne      *blank
074700020214     C                   EXSR      cntisv
074800991104X   2C                   ELSE
074900991104     C                   MOVEL     '1'           ERRL00
075000991104     C                   SETON                                        62
075100091117     C                   MOVEL     MSG(9)        DSMSMS
075200991104E   2C                   ENDIF
075300991104E   1C                   ENDIF
075400991008     C*--------------------
075500991008     C* tipo legame
075600991008     C*--------------------
075700991008IF  1C     ERRL00        IFEQ      '0'
075800020214     C*
075900100913     C* Verifico che il servizio del record corrente sia soggetto a controllo unificazioni
076000020214     C     VIDisv        lookup    isvkun                                 22
076100020214     C                   if        *in22 = *on
076200020214IF  2C     VIDtle        IFne      *blanks
076300091117     C     VIDtle        lookup    sktle                                  22
076400091117     C                   if        *in22 = *off
076500091117     C                   movel     '1'           errl00
076600091117     C                   seton                                        66
076700091117     C                   MOVEL     MSG(12)       DSMSMS
076800091117     C                   else
076900091117     C                   exsr      cnttle
077000091117     C                   endif
077100991008E   2C                   ENDIF
077200020214     C                   else
077300020214IF  2C     VIDtle        IFne      *blanks
077400020214     C                   MOVEL     '1'           ERRL00
077500020214     C                   SETON                                        66
077600020214     C                   MOVEL     MSG(28)       DSMSMS
077700020214E   2C                   ENDIF
077800020214     C                   endif
077900991008E   1C                   ENDIF
078000100913     C*
078100100913     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
078200100913     C     ERRL00        IFEQ      '0'
078300160705     C     VIDisv        lookup    isvftp                                 80
078400160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
078500160705     C                   setoff                                       80
078600160705     C                   endif
078700100913     C                   ENDIF
078800101022     C*
078900970528     C*--------------------
079000991008     C* codice padre
079100970528     C*--------------------
079200991105     C     *LIKE         DEFINE    vssksu        ksuVID
079300970528IF  1C     ERRL00        IFEQ      '0'
079400991105     C                   MOVEL     *BLANKS       VDDksu
079500991105IF  2C     VIDksu        IFEQ      *blanks                                      *STANDARD
079600020214IF  2C     VIDksu        OREQ      *zeros                                       *STANDARD
079700000705     C                   MOVEL     '1'           ERRL00
079800000705     C                   SETON                                        61
079900000705     C                   MOVEL     MSG(19)       DSMSMS
080000991105     C                   MOVEL     *ZEROS        ksuVID
080100970618X   2C                   ELSE
080200991105     C                   MOVEL     VIDksu        Wksu
080300991105     C                   MOVEL     *blank        Wdksu
080400991105     C                   EXSR      CHKksu
080500021114     C                   IF        flgKSU = 'S'
080600991105     C                   MOVEL     Wksu          VIDksu
080700991105     C                   MOVEL     WDksu         VDDksu
080800000728     C                   MOVEL     VIDksu        ksuVID
080900021114     C                   ENDIF
081000970618IF  3C     ERRL00        IFEQ      '1'
081100000728     C                   SETON                                        61
081200000728     C                   MOVEL     MSG(13)       DSMSMS
081300970618E   3C                   ENDIF
081400970618E   2C                   ENDIF
081500970528E   1C                   ENDIF
081600991015     C*--------------------
081700991015     C* DATA DECORRENZA
081800991015     C*--------------------
081900991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
082000991015IF  1C     ERRL00        IFEQ      '0'
082100991015     C                   Z-ADD     VIDDDE        wda8
082200991015     C                   Z-ADD     *ZEROS        da8w
082300991015     C                   EXSR      CHKDA8
082400991015     C                   Z-ADD     WDA8          viddde
082500991015     C                   Z-ADD     DA8W          ddevid
082600991015IF  2C     ERRL00        IFEQ      '1'
082700991015     C                   SETON                                        63
082800991015     C                   MOVEL     MSG(14)       dsmsms
082900991015E   2C                   ENDIF
083000991015E   1C                   ENDIF
083100991014     C* inserimento codice azienda (attualmente non abilitato)
083200991011IF  1C     ERRL00        IFEQ      '0'
083300991104IF  2C     VIDazp        IFne      *blank
083400991104     C                   SETON                                        60
083500991011     C                   MOVEL     MSG(19)       dsmsms
083600991011     C                   MOVEL     '1'           errl00
083700991011E   2C                   ENDIF
083800991011E   1C                   ENDIF
083900991011     C* INSERIMENTO -> RECORD INESISTENTE
084000991011IF  1C     ERRL00        IFEQ      '0'
084100991011IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
084200001116IF  2C     DEPOP1        OREQ      'O03'                                        *INSERIMENTO
084300991104     C                   MOVE      VIDisv        kceisv
084400001116     C                   MOVE      vidsun        kcesun
084500991104     C     KEYvss        CHAIN(N)  tivss01L                           99
084600970528IF  3C     *IN99         IFEQ      *OFF
084700970528     C                   MOVEL     '1'           ERRL00
084800970528     C                   MOVEL     MSG(8)        DSMSMS
084900970528     C                   SETON                                        606162
085000970528     C                   SETON                                        63
085100970528E   3C                   ENDIF
085200970528E   2C                   ENDIF
085300000317E   1C                   ENDIF
085400000317     C*---
085500000317     C* INSERIMENTO -> record inesistente come figlio di un cliente unificante per il servizio
085600010522     C*                record inesistente come padre (se stesso) con altro utente strategi
085700000317     C*---
085800000317     C* Cerca il padre se c'è
085900001116     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
086000001116     C     ERRL00        IFEQ      '0'
086100001116     C                   CLEAR                   tibs10ds
086200001116     C                   EVAL      d10drf = datcor                              *data riferimento
086300010522     C                   EVAL      d10tle = vidtle                              *legame INTERNET
086400001116     C                   EVAL      d10paf = 'P'                                 *se esiste record
086500001116     C                   MOVE      ksuvid        n8                8 0
086600001116     C                   Z-ADD     n8            d10cod                         *cliente figlio
086700001116     C                   CALL      'TIBS10R'                                    *cerca figli
086800001116     C                   PARM                    tibs10ds
086900001116     C                   IF        d10err = ' '                                 *esiste padre
087000001116     C                   MOVE      d10cop        kceksu
087100001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
087200000317     C     KE2vss        SETLL     tivss02L
087300000317     C     KE2vss        READE     tivss02L                               99
087400001116     C     *IN99         DOWEQ     *OFF
087500001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
087600001116     C     ddevid        ANDLE     vssdsc
087700001116     C                   MOVEL     MSG(10)       DSMSMS
087800001116     C                   LEAVE
087900001116     C                   ENDIF
088000001116     C     KE2vss        READE     tivss02L                               99
088100001116     C                   ENDDO
088200001116     C                   ELSE                                                   *NON esiste padre
088300000317     C                   MOVE      ksuvid        kceksu
088400001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
088500000317     C     KE2vss        SETLL     tivss02L
088600000317     C     KE2vss        READE     tivss02L                               99
088700001116     C     *IN99         DOWEQ     *OFF
088800001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
088900001116     C     ddevid        ANDLE     vssdsc
089000001116     C                   MOVEL     MSG(11)       DSMSMS
089100001116     C                   LEAVE
089200001116     C                   ENDIF
089300000317     C     KE2vss        READE     tivss02L                               99
089400001116e   4C                   ENDDO
089500001116e   3C                   ENDIF
089600001116e   2C                   ENDIF
089700001116e   1C                   ENDIF
089800970528     C*--------------------
089900970528     C* DATA SCADENZA
090000970528     C*--------------------
090100970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
090200970528IF  1C     ERRL00        IFEQ      '0'
090300970528     C                   Z-ADD     VIDDSC        WDA8
090400970528     C                   Z-ADD     *ZEROS        DA8W
090500970528     C                   EXSR      CHKDA8
090600970528     C                   Z-ADD     WDA8          VIDDSC
090700970528     C                   Z-ADD     DA8W          DSCVID
090800970528IF  2C     ERRL00        IFEQ      '1'
090900970528     C                   SETON                                        64
091000970528     C                   MOVEL     MSG(14)       DSMSMS
091100970528E   2C                   ENDIF
091200970528E   1C                   ENDIF
091300970528     C* SCADENZA > DECORRENZA
091400970528IF  1C     ERRL00        IFEQ      '0'
091500970528IF  2C     DSCVID        IFLT      DDEVID
091600970619     C                   SETON                                        64
091700970528     C                   MOVEL     MSG(15)       DSMSMS
091800970528     C                   MOVEL     '1'           ERRL00
091900970528E   2C                   ENDIF
092000970528E   1C                   ENDIF
092100970528     C* SCADENZA >= DATA CORRENTE
092200010220IF  1C*    ERRL00        IFEQ      '0'
092300010220IF  2C*    DEPOP1        IFEQ      'O02'
092400010220     C*    DEPOP1        OREQ      'O07'
092500010220     C*    DEPOP1        OREQ      'F06'
092600010220IF  3C*    DSCVID        IFLT      DATCOR
092700010220     C*                  SETON                                        64
092800010220     C*                  MOVEL     MSG(16)       DSMSMS
092900010220     C*                  MOVEL     '1'           ERRL00
093000010220E   3C*                  ENDIF
093100010220E   2C*                  ENDIF
093200010220E   1C*                  ENDIF
093300970528     C*---
093400970528     C* DECORRENZA + SCADENZA
093500970528     C*---
093600970528     C* ACCAVALLAMENTO DATI
093700970528IF  1C     ERRL00        IFEQ      '0'
093800080110     C* Controllo solo x servizi diversi da tipo T&T
093900050118     C     vidisv        lookup    isvtt                                  22
094000050118     C                   if        *in22 = *off
094100991104     C                   MOVEL     VIDisv        KCEisv
094200050118     C                   move      ksuVID        KCEksu
094300040910     C     Ke2vss        SETLL     tivss02L
094400040910     C     Ke2vss        READE(N)  tivss02L                               99
094500970528DO  2C     *IN99         DOWEQ     *OFF
094600080110IF  3C                   IF        (DEPOP1<>'F06' AND DDEVID<>vssDDE) OR
094700080110     C                              DEPOP1= 'F06'
094800080110     C                   IF        (DDEVID>=vssDDE AND DDEVID<=vssDSC) OR
094900080110     C                             (DSCVID>=vssDDE AND DSCVID<=vssDSC) OR
095000080110     C                             (vssDDE>=DDEVID AND vssDSC<=DSCVID)
095100970528     C                   SETON                                        606162
095200970619     C                   SETON                                        6364
095300970528     C                   MOVEL     MSG(17)       DSMSMS
095400970528     C                   MOVEL     '1'           ERRL00
095500970528     C                   LEAVE
095600970528E   5C                   ENDIF
095700970528E   3C                   ENDIF
095800040910     C     Ke2vss        READE(N)  tivss02L                               99
095900970528E   2C                   ENDDO
096000970528E   1C                   ENDIF
096100050118     C                   endif
096200020214     C*
096300020214     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
096400020214     C     vidisv        lookup    isvkun                                 22
096500020214     C                   if        *in22 = *on
096600000131     C*--------------------
096700000131     C* Clienti figli collegati
096800000131     C*--------------------
096900000215     C                   MOVEL     'N'           $esikun                        *non esistono kun
097000001116     C                   CLEAR                   vidmsg
097100020212     C     ERRL00        IFEQ      '0'
097200020212     C     VIDtle        IFNE      *blanks                                      *c'è legame
097300000204     C                   MOVEL     vidtle        kkutle
097400000131     C                   MOVEL     *blanks       kkuazp
097500000204     C                   MOVE      vidksu        kkucop
097600000215     C     KEYkun        SETLL     tikun01l
097700000215     C     KEYkun        READE     tikun01l                               99
097800020212     C                   DOW       NOT *in99
097900020212     C     kunvat        IFEQ      *blanks
098000020212     C                   IF        kundsc <  ddevid  or                         *non valido
098100010522     C                             kundde >  dscvid
098200020212     C                   ELSE
098300060802     C                   IF        kundsc >= datcor
098400000215     C                   MOVEL     'S'           $esikun                        *esistono kun
098500001116     C                   LEAVE
098600060802     C                   ENDIF
098700020212     C                   ENDIF
098800020212     C                   ENDIF
098900000215     C     KEYkun        READE     tikun01l                               99
099000020212     C                   ENDDO
099100020212     C     $esikun       IFEQ      'N'                                          *non esistono kun
099200060802     C                   IF        kundsc >= datcor
099300020212     C                   MOVEL     des(1)        vidmsg
099400001116     C                   SETON                                        76        *attenzione
099500060802     C                   ENDIF
099600020212     C                   ENDIF
099700000215     C*
099800020212     C                   ELSE                                                   *non c'è legame
099900000215     C*
100000000215     C                   MOVE      vidksu        kkucop
100100000215     C     KE3kun        SETLL     tikun03l
100200000215     C     KE3kun        READE     tikun03l                               99
100300020212     C                   DOW       NOT *in99
100400020212     C     kunvat        IFEQ      *blanks
100500020212     C                   IF        kundsc <  ddevid  or                         *non valido
100600010522     C                             kundde >  dscvid
100700020212     C                   ELSE
100800000215     C                   MOVEL     'S'           $esikun                        *esistono kun
100900001116     C                   LEAVE
101000020212     C                   ENDIF
101100020212     C                   ENDIF
101200000808     C     KEYkun        READE     tikun01l                               99
101300020212     C                   ENDDO
101400020212     C     $esikun       IFEQ      'S'                                          *esistono kun
101500060802     C                   IF        kundsc >= datcor
101600001116     C                   MOVEL     des(2)        vidmsg
101700020212     C                   SETON                                        76        *attenzione
101800060802     C                   ENDIF
101900020212     C                   ENDIF
102000020212     C                   ENDIF
102100020212     C                   ENDIF
102200020211     C*--------------------
102300091027     C* Che nn sia cliente figlio
102400020211     C*--------------------
102500091117     C*    ERRL00        ifeq      '0'
102600091117     C*                  move(p)   vidksu        kkucof
102700091117     c*
102800091117     c* ciclo x tutti i legami d tipo "internet"
102900091117     c*                  z-add     1             jtle
103000091117     c*                  dow       jtle <= %elem(sktle)
103100091117     c*                  if        sktle(jtle) <> *blanks
103200091117     c*
103300091117     C*                  movel     sktle(jtle)   kkutle
103400091117     C*    ke6kun        setll     tikun06l
103500091117     C*    ke6kun        reade     tikun06l
103600091117     C*                  dow       not %eof(tikun06l)
103700091117     C*    kunvat        ifeq      *blanks                                      *annullati/sospesi
103800091117     C*                  if        kundsc <  ddevid  or                         *non valido
103900091117     C*                            kundde >  dscvid
104000091117     C*                  else                                                   *valido
104100091117     C*                  seton                                        61
104200091117     C*                  movel     MSG(27)       DSMSMS
104300091117     C*                  movel     '1'           ERRL00
104400091117     C*                  leave
104500091117     C*                  endif
104600091117     C*                  endif
104700091117     C*    ke6kun        reade     tikun06l
104800091117     C*                  enddo
104900091027     c*
105000091117     c*                  else
105100091117     c*                  leave
105200091117     c*                  endif
105300091117     c*
105400091117     c*                  add       1             jtle
105500091117     c*                  enddo
105600091117     C*                  endif
105700091117     C*
105800091117     C                   endif
105900000913     C*------------------------
106000010522     C* Verifico che in INS/COPIA non esista già il cliente per il tipo servizio DL (x dec+scad)
106100000913     C*------------------------
106200020212     C     ERRL00        IFEQ      '0'
106300010522     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') AND
106400010522     C                              VIDisv = 'DL'
106500000913     C                   MOVE(p)   vidksu        kceksu
106600001116     C                   MOVE(p)   vidisv        kceisv                         *stesso servizio
106700000913     C     KE2vss        SETLL     tivss02L
106800000913     C     KE2vss        READE     tivss02L                               99
106900020212     C     *IN99         DOWEQ     *OFF
107000020212     C     ddevid        IFGE      vssdde                                       *in decorrenza
107100001116     C     ddevid        ANDLE     vssdsc
107200001116     C                   MOVEL     MSG(24)       DSMSMS
107300000913     C                   MOVEL     '1'           ERRL00
107400001116     C                   LEAVE
107500020212     C                   ENDIF
107600000913     C     KE2vss        READE     tivss02L                               99
107700020212     C                   ENDDO
107800020212     C                   ENDIF
107900020212     C                   ENDIF
108000000731     C*--------------------
108100000731     C* codice strategi
108200000731     C*--------------------
108300020802     C                   MOVEL     VIDsun        depTIP            1
108400020802     C*
108500020802     C* Eseguo controllo compatibilità Codice STRATEGI rispotto al servizio indicato
108600020802     C     ERRL00        IFEQ      '0'
108700020802     C                   IF        depTIP = '9' and
108800020802     C                             §isvproc <> 'F'
108900020802     C                   SETON                                        6265
109000020802     C                   MOVEL     MSG(29)       DSMSMS
109100020802     C                   MOVEL     '1'           ERRL00
109200020802     C                   ENDIF
109300020802     C                   IF        depTIP <> '9' and
109400020802     C                             §isvproc <> 'S'
109500020802     C                   SETON                                        6265
109600020802     C                   MOVEL     MSG(29)       DSMSMS
109700020802     C                   MOVEL     '1'           ERRL00
109800020802     C                   ENDIF
109900020802     C                   ENDIF
110000020802     C*
110100020802     C* Eseguo controllo esistenza Codice STRATEGI solo se "sun" nn inizia x '9'
110200020212     C     ERRL00        IFEQ      '0'
110300020802     C                   IF        depTIP <> '9'
110400000731     C                   EXSR      scanfile
110500020212     C                   ENDIF
110600020802     C                   ENDIF
110700010605     C*
110800010605     C* SE QUI TUTTO OK E TIPO SERVIZIO = 'TT' ESEGUO CONTROLLO ED EVENTUALE INSERIMNETO
110900010605     C* ANCHE DEL RECORD PER GLI ORM DA INTERNET NELL'ABILITAZIONE TIPI FILE X CLIENTE (TIVIR00F)
111000010608     C     ERRL00        IFEQ      '0'
111100010608     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') and
111200010608     C                             VIDisv = 'TT'
111300010626 ORM C*                  EXSR      DECVIR
111400010605     C                   ENDIF
111500010605     C                   ENDIF
111600090804     C*--------------------
111700100217     C* SUBK / tipo servizio
111800090804     C*--------------------
111900090804     C     ERRL00        IFEQ      '0'
112000090804     C                   IF        VIDSUBK <> *blanks AND VIDISV <> 'TT'
112100090804     C                   MOVEL     '1'           ERRL00
112200090804     C                   SETON                                        67
112300090804     C                   MOVEL     MSG(30)       DSMSMS
112400090804     C                   ENDIF
112500090804     C                   ENDIF
112600100217     C*--------------------
112700100217     C* controlli specifici x servizio
112800100217     C*--------------------
112900100217     C     ERRL00        IFEQ      '0'
113000100217     C                   z-add     1             jISV              3 0
113100100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
113200100217     C                   if        *in22 = *on
113300100217     C                   if        isvCTRL(jISV) <> *blanks
113400100217     C                   select
113500100913     C* - 0001
113600100322     C                   when      isvCTRL(jISV) = '0001'
113700120327     C*
113800120328     C* se cliente annullato o bloccato per motivi di mancato pagamento
113900120327     C                   IF        ACOFlg <> *blanks
114000130314     C                             or ACOAbl <> *blanks
114100120327     C                   SETON                                        61
114200120327     C                   EVAL      ErrL00 = '1'
114300120327     C                   MOVEL     MSG(37)       DSMSMS
114400120327     C                   ENDIF
114500120327     C*
114600120327     C     ERRL00        IFEQ      '0'
114700100217     C                   eval      kceksu = vidksu
114800100217     C                   eval      kceisv = 'TT'
114900100217     C                   eval      kcesun = vidsun
115000100217     C     KEYvss02_C    chain     tivss02L
115100100217     C                   if        %found(tivss02l) and
115200100217     C                             vssdde <= datcor and
115300100217     C                             vssdsc >  datcor
115400100322     C*
115500100322     C     FUNKEY        IFEQ      '06'
115600100406     C*
115700100406      /FREE
115800100406
115900100406              Ub§3EWupd_Upd (VIDKSU : VIDSUN : 'A' : *off );
116000100406
116100100406
116200100406      /END-FREE
116300100322     C*
116400100406     C                   ENDIF
116500100217     C                   else
116600100217     C                   MOVEL     '1'           ERRL00
116700100217     C                   SETON                                        62
116800100913     C                   MOVEL     MSG(31)       DSMSMS
116900100217     C                   endif
117000120327     C                   endif
117100101006     C* Infine rileggo il record "originale"
117200101006     C                   eval      kceksu = vidksu
117300101006     C                   eval      kceisv = vidisv
117400101006     C                   eval      kcesun = vidsun
117500101006     C     KEYvss02_C    chain     tivss02L
117600100913     C*
117700100217     C                   endsl
117800100217     C                   endif
117900100217     C                   endif
118000100217     C                   ENDIF
118100100913     C*--------------------
118200100913     C* Password FTP
118300100913     C*--------------------
118400100913     C     ERRL00        IFEQ      '0'
118500100913     C                   IF        *IN80 = *on
118600100913     C                   IF        VIDPWDFTP = *blanks
118700100913     C                   MOVEL     '1'           ERRL00
118800100913     C                   SETON                                        68
118900100913     C                   MOVEL     MSG(32)       DSMSMS
119000100913     C                   ELSE
119100100913     C*
119200100913     C* Password => verifica caratteri consentiti
119300100913     C                   Z-ADD     1             pos               2 0
119400100913     C                   FOR       pos = 1 to %len(%trim(VIDPWDFTP))
119500100913     C                   IF        %lookup(%subst(%trim(VIDPWDFTP):pos:1)
119600100913     C                             :valAry) = *zeros
119700100913     C                   MOVEL     '1'           ERRL00
119800100913     C                   SETON                                        68
119900100913     C                   MOVEL     MSG(33)       DSMSMS
120000100913     C                   ENDIF
120100100913     C                   ENDFOR
120200100913     C*
120300100913     C                   IF        ERRL00 <> '1'
120400100913     C                   IF        %len(%trim(VIDPWDFTP)) < 8
120500100913     C                   MOVEL     '1'           ERRL00
120600100913     C                   SETON                                        68
120700100913     C                   MOVEL     MSG(34)       DSMSMS
120800100913     C                   ENDIF
120900100913     C                   ENDIF
121000100913     C                   ENDIF
121100100913     C                   ENDIF
121200100913     C                   ENDIF
121300100914     C*--------------------
121400100914     C* Verifica Servizio / "F18"
121500100914     C*--------------------
121600100914     C     ERRL00        IFEQ      '0'
121700100914     C                   IF        VIDISV <> 'IE' and FUNKEY = '18'
121800100914     C                   MOVEL     '1'           ERRL00
121900100914     C                   SETON                                        68
122000100914     C                   MOVEL     MSG(35)       DSMSMS
122100100914     C                   ENDIF
122200100914     C                   ENDIF
122300060214     C*
122400100217     C* Forzatura necessaria
122500060214     C                   IF        KNMUS = 'EDPFG'
122600060214     C                   SETOFF                                       9091
122700060214     C                   ENDIF
122800000731     C*
122900970528     C                   ENDSR
123000010605     C*------------------------------------------------------------------------*
123100010608     C* DECVIR - CONTROLLA CAMPI DA INSERIRE NEL FILE "REGOLE X CLIENTE" (TIVIR00F)
123200010605     C*------------------------------------------------------------------------*
123300010605     C     DECVIR        BEGSR
123400010608     C*
123500010608     C                   MOVEL     '0'           ERRL01
123600010605     C*
123700010605     C*--------------------
123800010605     C* INSERIMENTO -> RECORD INESISTENTE
123900010605     C*--------------------
124000010608     C     ERRL01        IFEQ      '0'
124100010605     C                   MOVE(P)   VIDksu        KCEksc
124200010605     C                   MOVEL     'TO'          KCEtip
124300010605     C                   Z-ADD     ddeVID        KCEdti
124400010605     C     KEYvi2        CHAIN(N)  tivir03L                           99
124500010605     C     *IN99         IFEQ      *ON
124600010605     C* DECORRENZA + SCADENZA  (ACCAVALLAMENTO DATI)
124700010605     C     KEYvir        SETLL     tivir03L
124800010605     C     KEYvir        READE(N)  tivir03L                               99
124900010605     C     *IN99         DOWEQ     *OFF
125000010605     C     ddeVID        IFGE      virdti                                       *DECORRENZA
125100010605     C     ddeVID        ANDLE     virdtf
125200010605     C     dscVID        ORGE      virdti                                       *SCADENZA
125300010605     C     dscVID        ANDLE     virdtf
125400010605     C     virdti        ORGE      ddeVID                                       *DECORRENZA
125500010605     C     virdtf        ANDLE     dscVID
125600010605     C                   MOVEL     MSG(25)       DSMSMS
125700010608     C                   MOVEL     '1'           ERRL01
125800010605     C                   LEAVE
125900010605     C                   ENDIF
126000010605     C     KEYvir        READE(N)  tivir03L                               99
126100010605     C                   ENDDO
126200010605     C                   ELSE
126300010605     C                   MOVEL     MSG(26)       DSMSMS
126400010608     C                   MOVEL     '1'           ERRL01
126500010605     C                   ENDIF
126600010605     C                   ENDIF
126700010605     C*
126800010605     C                   ENDSR
126900010605     C*------------------------------------------------------------------------*
127000010605     C* SCANFILE - Routine di scansione del file degli UTENTI di STRATEGI
127100010605     C*------------------------------------------------------------------------*
127200010605     C     SCANFILE      BEGSR
127300000719     C*
127400000719     C                   open      sgius
127500000719     C*
127600000719     C                   eval      chk = '1'
127700000719     C                   add       1             giro
127800130611     C     vidsun        chain(e)  sgius
127900130611     C                   if        %found(sgius)
128000130611     C                   eval      chk = '0'
128100130611     C                   endif
128200000719     C*
128300000719     C                   if        chk = '1'
128400000719     C                   movel     MSG(21)       DSMSMS                         *MESSAGGIO
128500000719     C                   endif
128600000719     C*
128700000719     C                   close     sgius
128800000719     C*
128900010605     C                   ENDSR
129000991008     C*------------------------------------------------------------------------*
129100000807     C* Cntisv - CONTROLLA codice tipo servizio a cui il cliente è abilitato
129200991008     C*------------------------------------------------------------------------*
129300001116     C     cntisv        begsr
129400001116     C                   clear                   TIBS02DS
129500001116     C*
129600991104     C     '?'           SCAN      Vidisv                                 30
129700001116     C*  Ricerca
129800001116     C                   eval      t02tla = 'L'
129900001116    1C                   IF        *IN30 = *ON
130000001116     C                   movel     'R'           t02mod
130100001116     C                   else
130200001116     C                   movel     'C'           t02mod
130300001116     C                   movel(p)  vidisv        t02ke1
130400001116     C                   end
130500001116     C                   movel     'ISV'         t02cod
130600001116     C                   movel     knsif         t02sif
130700991008     C                   CALL      'TIBS02R'
130800001116     C                   parm                    KPJBA
130900001116     C                   parm                    TIBS02DS
131000001116     C
131100001116     C*
131200991008    2C                   IF        T02err = *blanks
131300991104     C                   movel     T02ke1        Vidisv
131400020802     C                   movel(p)  T02uni        DSISV
131500020802     C                   movel     §isvdesc      vddisv
131600001116     C                   else
131700001116     C                   movel     '1'           errl00
131800001116     C                   seton                                        62
131900991104     C                   MOVEL     MSG(9)        DSMSMS
132000991008     C                   ENDIF
132100001116     C
132200001116     C                   endsr
132300000807     C*------------------------------------------------------------------------*
132400991104     C* Cnttle - CONTROLLA codice tipo legame
132500991104     C*------------------------------------------------------------------------*
132600001116     C     cnttle        begsr
132700001116     C                   clear                   TIBS02DS
132800001116     C*
132900991104     C     '?'           SCAN      Vidtle                                 30
133000001116     C*  Ricerca
133100001116     C                   eval      t02tla = 'L'
133200991104    1C                   IF        *IN30 = *ON
133300001116     C                   movel     'R'           t02mod
133400001116     C                   else
133500001116     C                   movel     'C'           t02mod
133600001116     C                   movel(p)  vidtle        t02ke1
133700001116     C                   end
133800991104     C                   movel     'TLE'         t02cod
133900001116     C                   movel     knsif         t02sif
134000001116     C                   CALL      'TIBS02R'
134100001116     C                   parm                    KPJBA
134200001116     C                   parm                    TIBS02DS
134300001116     C
134400001116     C*
134500991104    2C                   IF        T02err = *blanks
134600000807     C                   exsr      chkaut
134700000807     C                   if        errl00 = *blanks
134800991104     C                   movel     T02ke1        Vidtle
134900091027     C                   movel     T02uni        dstle
135000991104     C                   movel     §tledesc      Vddtle
135100000807     C                   endif
135200000807     C                   else
135300000807     C                   movel     '1'           errl00
135400000807     C                   seton                                        66
135500000807     C                   MOVEL     MSG(12)       DSMSMS
135600991104     C                   ENDIF
135700000807     C
135800000807     C                   endsr
135900000807     C*------------------------------------------------------------------------*
136000000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
136100000807     C*------------------------------------------------------------------------*
136200000807     C     CHKAUT        BEGSR
136300000807     C*
136400020903     C* VERIFICO CHE L'UTENTE CHE EFFETTUA LA RICHIESTA SIA UN EDP*
136500020903     C                   IF        %subst(KNMUS:1:3) = 'EDP'
136600020903     C                   ELSE
136700020903     C                   MOVEL     '1'           ERRL00
136800020903     C                   MOVEL     MSG(23)       DSMSMS
136900020903     C                   ENDIF
137000000807     C*
137100000807     C                   ENDSR
137200970528     C*------------------------------------------------------------------------*
137300991105     C* CHKksu - CONTROLLA codice padre figlio
137400970528     C*------------------------------------------------------------------------*
137500991105     C     CHKksu        BEGSR
137600970528     C*
137700991105     C     *LIKE         DEFINE    VIDksu        Wksu
137800991105     C     *LIKE         DEFINE    VDDksu        WDksu
137900001116     C                   MOVE      *zeros        Wksc
138000021114     C                   MOVEL     'S'           flgKSU            1
138100970528     C*
138200970528     C* RICHIESTO ELENCO
138300970528     C                   SETOFF                                           28
138400991105     C     '?'           SCAN      Wksu                                   28
138500970528IF  1C     *IN28         IFEQ      *ON
138600991008     C* RICHIESTO ELENCO CODICI
138700021114     C                   MOVEL     'N'           flgKSU
138800001116     C* CLIENTI
138900991008     C                   CLEAR                   DSXC
139000020924     C                   Z-ADD     DUTKCI        DXCCAP
139100991008     C                   CALL      'XCLIR'
139200991008     C                   PARM                    DSXC
139300991008     C* SCOMPATTA
139400991008     C                   MOVEA     DXCKSC        KSA                        ALF
139500991008DO  2C     1             DO        30            J                 3 0
139600991008     C                   MOVE      KSA(J)        DSKSA                      F
139700991008     C                   Z-ADD     DSKSN         KSC(J)                     M
139800991008E   2C                   ENDDO
139900991011     C                   MOVEL     KSC(1)        WKSC              7
140000060807     C                   MOVEL     *all'0'       WKSU
140100060807     C                   MOVE      WKSC          WKSU
140200991011     C                   MOVEL     '1'           $TESVD            1        TA
140300060807E   1C                   ENDIF
140400991008     C*
140500991008     C* CODICE CLIENTE NUMERICO
140600021114IF  1C     WKSC          IFLT      *ZEROS
140700021114     C                   MOVEL     '1'           WERR              1
140800021114X   1C                   ELSE
140900021114     C*
141000970528     C* CONTROLLA ESISTENZA CODICE
141100021114     C                   MOVEL     'S'           flgKSU
141200991210     C                   MOVE      Wksu          wcli              7 0
141300001116     C                   EXSR      dectib
141400021114E   1C                   ENDIF
141500970528     C*
141600970528     C                   ENDSR
141700991210     C*------------------------------------------------------------------------*
141800001116     C* DECTIB - Controlla esistenza codice cliente
141900991210     C*------------------------------------------------------------------------*
142000001116     C     DECTIB        BEGSR
142100001116     C*
142200010913     C     VIDksu        CHAIN     tianv01l
142300010913     C                   IF        %found(tianv01l)
142400010913     C                   MOVEL(p)  anvdes        WDKsu
142500010913     C                   ELSE
142600050909     C*
142700050909     C* SOLO CARATTERI NUMERICI
142800050909     C   28              MOVE      *all'0'       wksu
142900050909     C   28              MOVE      wksc          wksu
143000050909     C                   SETOFF                                       28
143100050909     C                   TESTN                   Wksu                 28
143200050909IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
143300120327     C                   MOVEL     '1'           ERRL00
143400050909X   2C                   ELSE                                                   *E'NUMERO DA TESTN
143500050909     C                   MOVE      Wksu          W001              1
143600050909IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
143700050909     C                   MOVEL     '1'           ERRL00
143800050909X   3C                   ELSE                                                   *NUMERO
143900991210     C                   CLEAR                   BS69DS
144000991210     C                   CLEAR                   ACODS
144100991210     C                   MOVEL     KNSIF         I69SIF
144200991210     C                   Z-ADD     WCLI          I69KAC
144300991210     C                   CALL      'TIBS69R'
144400991210     C                   PARM                    BS69DS
144500991210     C                   PARM                    ACODS
144600991210     C                   PARM                    INDDS
144700991210     C                   PARM                    CLPDS
144800991210     C                   PARM                    CLSDS
144900991210     C     O69ERR        IFNE      '1'
145000991210     C                   MOVEL     ACORAG        wdksu
145100991210     C                   ELSE
145200000728     C                   MOVEL     *ALL'*'       Wdksu
145300001116     C                   MOVE      '1'           errl00
145400021114     C                   MOVEL     'N'           flgKSU
145500001116     C                   ENDIF
145600050909E   3C                   ENDIF
145700050909E   2C                   ENDIF
145800991210     C                   ENDIF
145900001116     C*
146000001116     C                   ENDSR
146100991210     C*------------------------------------------------------------------------*
146200991210     C* FINSR - OPERAZIONI FINALI
146300991210     C*------------------------------------------------------------------------*
146400991210     C     FINSR         BEGSR
146500991210     C*
146600991210     C* CHIUDE I FILE DEI PGM CHIAMATI
146700991210     C                   CLEAR                   bs69ds
146800991210     C                   CLEAR                   acods
146900991210     C                   CLEAR                   indds
147000991210     C                   CLEAR                   clpds
147100991210     C                   CLEAR                   clsds
147200991210     C                   MOVEL     'C'           i69tla
147300991210     C                   MOVEL     knsif         i69sif
147400991210     C                   CALL      'TIBS69R'
147500991210     C                   PARM                    bs69ds
147600991210     C                   PARM                    acods
147700991210     C                   PARM                    indds
147800991210     C                   PARM                    clpds
147900991210     C                   PARM                    clsds
148000991210     C*
148100991210     C                   ENDSR
148200970528     C*------------------------------------------------------------------------*
148300970528     C* CHKDA8 - CONTROLLA LA DATA (8)
148400970528     C*------------------------------------------------------------------------*
148500970528     C     CHKDA8        BEGSR
148600970528     C*
148700970528     C     *LIKE         DEFINE    VIDDDE        WDA8
148800970528     C     *LIKE         DEFINE    VIDDDE        DA8W
148900970528     C*
149000970528     C                   Z-ADD     WDA8          G08DAT
149100970528     C                   Z-ADD     *ZEROS        G08INV
149200970528     C                   MOVEL     '0'           G08ERR
149300970528     C                   CALL      'XSRDA8'
149400970528     C                   PARM                    WLBDA8
149500970528     C                   Z-ADD     G08DAT        WDA8
149600970528     C                   Z-ADD     G08INV        DA8W
149700970528IF  1C     G08ERR        IFEQ      '1'
149800970528     C                   MOVEL     '1'           ERRL00
149900970528E   1C                   ENDIF
150000970528     C*
150100970528     C                   ENDSR
150200970526     C*------------------------------------------------------------------------*
150300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
150400970526     C*------------------------------------------------------------------------*
150500970526     C     IREVID        BEGSR
150600970526     C* CAMPI
150700991104     C                   MOVEL     vsstle        VIDtle
150800991104     C                   MOVEL     vssisv        VIDisv
150900991104     C                   MOVEL     vssazp        VIDazp
151000991105     C                   MOVEL     vsssun        VIDsun
151100001116     C                   MOVE      vssksu        VIDksu
151200090804     C                   MOVEL     vssDTA        DVSSDTA
151300090804     C                   MOVEL     §VSSSUBK      VIDSUBK
151400991104     C                   Z-ADD     vssDDE        G08INV
151500970528     C                   Z-ADD     *ZEROS        G08DAT
151600970528     C                   MOVEL     '3'           G08ERR
151700970528     C                   CALL      'XSRDA8'
151800970528     C                   PARM                    WLBDA8
151900970528     C                   Z-ADD     G08DAT        VIDDDE
152000991104     C                   Z-ADD     vssDSC        G08INV
152100970528     C                   Z-ADD     *ZEROS        G08DAT
152200970528     C                   MOVEL     '3'           G08ERR
152300970528     C                   CALL      'XSRDA8'
152400970528     C                   PARM                    WLBDA8
152500051013     C                   Z-ADD     G08DAT        VIDDSC
152600051013     C*
152700051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
152800051013     C                   IF        DEPOP1 = 'F06' OR
152900051013     C                             DEPOP1 = 'O03' OR
153000051013     C                             DEPOP1 = 'O07'
153100051013     C                   EVAL      VIDDDE = DATCORINV
153200060726     C                   EVAL      VIDDSC = 31122039
153300051013     C                   ENDIF
153400970527     C*
153500970526     C                   ENDSR
153600970526     C*------------------------------------------------------------------------*
153700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
153800970526     C*------------------------------------------------------------------------*
153900970526     C     AZZVID        BEGSR
154000970526     C* CAMPI
154100000316     C*---               MOVEL     *BLANKS       VIDtle
154200991104     C                   MOVEL     *BLANKS       VIDisv
154300100913     C                   MOVEL     *BLANKS       VIDpwdftp
154400000316     C*---               MOVEL     *BLANKS       VIDazp
154500000316     C*---               Z-ADD     *ZEROS        VIDDDE
154600000316     C*---               Z-ADD     *ZEROS        VIDDSC
154700000316     C*---               move      *ZEROS        VIDksu
154800000316     C*---               move      *ZEROS        VIDsun
154900970526     C* DECODIFICHE
155000000316     C*---               MOVEL     *BLANKS       VDDtle
155100000215     C                   MOVEL     *BLANKS       VIDmsg
155200000316     C*---               MOVEL     *BLANKS       VDDksu
155300000316     C                   MOVEL     *BLANKS       VDDisv
155400970526     C*
155500970526     C                   ENDSR
155600970526     C*------------------------------------------------------------------------*
155700991104     C* IMPvss - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
155800970526     C*------------------------------------------------------------------------*
155900991104     C     IMPvss        BEGSR
156000970526     C*
156100991104     C                   MOVE      VIDazp        vssazp                         *CHIAVE
156200001116     C                   MOVE      ksuVID        vssksu
156300090804     C                   MOVEL     VIDSUBK       §VSSSUBK
156400090831     C                   EVAL      vssDTA = DVSSDTA
156500001116     C                   MOVE      VIDtle        vsstle
156600090714     C                   MOVE      VIDisv        vssISV
156700991104     C                   Z-ADD     DDEVID        vssDDE
156800991104     C                   Z-ADD     DSCVID        vssDSC
156900970526     C*
157000970526     C                   ENDSR
157100010605     C*------------------------------------------------------------------------*
157200010605     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE DELLE ABILITAZIONI CLIENTI/TIPI FILE (TIVIR)
157300010605     C*------------------------------------------------------------------------*
157400010605     C     IMPvir        BEGSR
157500010605     C*
157600010605     C                   MOVE      ksuVID        virksc
157700010605     C                   MOVE      'TO'          virtip
157800010605     C                   MOVE      'TT'          virfi1
157900010605     C                   MOVE      001           virseq
158000010605     C                   MOVE      999           virpoi
158100010605     C                   EVAL      virdes = 'ORM DA INTERNET'
158200010605     C                   MOVEL     'G '          virfi2
158300010605     C                   Z-ADD     DDEVID        virdti
158400010605     C                   Z-ADD     DSCVID        virdtf
158500010605     C*
158600010605     C                   ENDSR
158700970526     C*------------------------------------------------------------------------*
158800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
158900970526     C*------------------------------------------------------------------------*
159000970526     C     IMPFUN        BEGSR
159100970526     C*
159200970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
159300970526IF  1C     DEPOP1        IFEQ      'O05'
159400970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
159500970526     C                   MOVEL     'N'           FUDATT
159600970526     C                   MOVEL     FUDRIG        FUD(6)
159700970526E   1C                   ENDIF
159800100914     C*
159900100914     C* SE NO INSERIMENTO e NO COPIOA => NO "F18"
160000100914     C                   IF        DEPOP1 <> 'F06' AND
160100100914     C                             DEPOP1 <> 'O03'
160200111216     C* NO F18 anche se l'AS400 non è SETRAS
160300111216     C                             or  AS400_ok = *off
160400100914     C                   MOVEL     FUD(18)       FUDRIG                         *AGGIORNAMENTO
160500100914     C                   MOVEL     'N'           FUDATT
160600100914     C                   MOVEL     FUDRIG        FUD(18)
160700100914     C                   ENDIF
160800970526     C*
160900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
161000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
161100970526     C                   EXSR      FUNGES
161200970526     C*
161300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
161400970526     C*---
161500970526     C* 90 -> PROTEZIONE CHIAVE
161600970526     C* 91 -> PROTEZIONE TOTALE
161700030704     C* 92 -> PROTEZIONE PARZIALE
161800970526     C*---
161900970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
162000970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
162100970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
162200970526     C                   SETOFF                                       91
162300970526E   2C                   ENDIF
162400970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
162500970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
162600970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
162700970526E   2C                   ENDIF
162800970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
162900970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
163000970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
163100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
163200970526     C                   SETON                                        91
163300970526E   2C                   ENDIF
163400970526X   1C                   ELSE                                                   *I N T E R R O G A
163500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
163600970526     C                   SETON                                        91
163700970526E   1C                   ENDIF
163800030704     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
163900030704     C                   SETON                                        92        *SPROTEZIONE SCAD.
164000030704     C                   ENDIF
164100970526     C*
164200970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
164300970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
164400991104     C                   MOVEL     s20isv        KCEisv
164500001116     C                   MOVE      s20sun        KCEsun
164600991104     C     KEYvss        CHAIN     tivss01L                           9998
164700001116IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
164800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
164900970526     C                   MOVEL     MSG(6)        DSMSMS
165000970526X   2C                   ELSE
165100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
165200980128IF  3C     DEPOP1        IFEQ      'O05'
165300001116     C     DEPOP1        OREQ      'O02'
165400101022     C*
165500101022     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
165600160705     C     vssISV        lookup    isvftp                                 80
165700160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
165800160705     C                   setoff                                       80
165900160705     C                   endif
166000101022     C                   IF        *IN80
166100111216     C                             and AS400_ok = *on
166200101022      /FREE
166300101022
166400101022         if UBBVSQL01_RtvUtente(
166500101022                         pInConn
166600101022                       : %subst(vssKSU:2:7)
166700101022                       : pOutNumRec
166800101022                       : pOutUsrPwd
166900101022                       : pOutUsrPath
167000101022                       : pOutUsrDesc
167100101022                       : pOutUsrFAnn
167200101022                       : pOutUsrEmail
167300101022                       : pOutUsrNote
167400101022                       : pOutUsrFList) = 0;
167500101022              if pOutNumRec = 1;
167600101022                 VIDPWDFTP = pOutUsrPwd;
167700101022              endif;
167800101022         endif;
167900101022
168000101022      /END-FREE
168100101022     C
168200101022     C                   ENDIF
168300101022     C*
168400101022     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
168500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
168600980128E   3C                   ENDIF
168700970526E   2C                   ENDIF
168800970526X   1C                   ELSE                                                   *INSERIMENTO
168900970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
169000970526E   1C                   ENDIF
169100970526     C*
169200970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
169300970526IF  1C     DEPOP1        IFEQ      'O02'
169400970526     C                   MOVEL     DEC(1)        VIDFUN
169500970526E   1C                   ENDIF
169600991014IF  1C     DEPOP1        IFEQ      'O03'
169700991014     C                   MOVEL     DEC(6)        VIDFUN
169800991014E   1C                   ENDIF
169900970526IF  1C     DEPOP1        IFEQ      'O04'
170000970526     C                   MOVEL     DEC(2)        VIDFUN
170100970526E   1C                   ENDIF
170200970526IF  1C     DEPOP1        IFEQ      'O05'
170300970526     C                   MOVEL     DEC(3)        VIDFUN
170400970526E   1C                   ENDIF
170500970526IF  1C     DEPOP1        IFEQ      'O07'
170600970526     C                   MOVEL     DEC(4)        VIDFUN
170700970526E   1C                   ENDIF
170800970526IF  1C     DEPOP1        IFEQ      'F06'
170900970526     C                   MOVEL     DEC(5)        VIDFUN
171000970526E   1C                   ENDIF
171100970526     C*
171200970526     C                   ENDSR
171300020212     C*--------------------------------------------------------------------------------------------*
171400020212     C* CARTAB - carica le tabelle occorrenti
171500020212     C*--------------------------------------------------------------------------------------------*
171600020212     C     CARTAB        BEGSR
171700020212     C*
171800020212     C* Tipi servizi soggetti a controllo unificazioni
171900020212     C                   z-add     *zeros        x                 3 0
172000050118     C                   z-add     *zeros        y                 3 0
172100100217     C                   z-add     *zeros        k                 3 0
172200100913     C                   z-add     *zeros        ww                3 0
172300020212     C                   eval      tbecod = 'ISV'
172400091027     C     tbecod        setll     tntbe01l
172500091027     C     tbecod        reade     tntbe01l
172600020212     C                   dow       not %eof(tntbe01l)
172700020212     C                   movel     tbeuni        dsisv
172800020212     C                   if        §isvuni  = 'S'
172900020212     C                   add       1             x
173000020212     C                   eval      isvkun(x) = tbeke1
173100020212     C                   endif
173200080110     C                   if        §isvrgr  = 'T'
173300050118     C                   add       1             y
173400050118     C                   eval      isvtt(y) = tbeke1
173500050118     C                   endif
173600100913     C                   if        §isvftp  = 'F'
173700100913     C                   add       1             ww
173800100913     C                   eval      isvftp(ww) = tbeke1
173900100913     C                   endif
174000100217     C                   add       1             k
174100100217     C                   eval      isvKEYS(k)   = tbeke1
174200100217     C                   eval      isvCTRL(k)   = §ISVCTRL
174300101006     C                   eval      isvPGMAFT(k) = §ISVPGMSRV
174400091027     C     tbecod        reade     tntbe01l
174500020212     C                   enddo
174600091027     c*---
174700091027     c* tipi legami "internet"
174800091027     c*---
174900091027     c                   z-add     *zeros        jtle
175000091027     c                   movel     'TLE'         tbecod
175100091027     c     tbecod        setll     tntbe01l
175200091027     c     tbecod        reade     tntbe01l
175300091027     c                   dow       not %eof(tntbe01l)
175400091027     c                   if        tbeatb = *blanks
175500091027     c                   eval      dstle = tbeuni
175600091027     c* solo legami d tipo "internet"
175700091027     c                   if        §TLEGRP = 'W'
175800091027     c                   add       1             jtle
175900091027     c                   eval      sktle(jtle) = tbeke1
176000091027     c                   endif
176100091027     c                   endif
176200091027     c     tbecod        reade     tntbe01l
176300091027     c                   enddo
176400020212     C*
176500020212     C                   ENDSR
176600020924     C*--------------------------------------------------------------------------------------------*
176700020924     C* REPERISCE I DATI UTENTE
176800020924     C*--------------------------------------------------------------------------------------------*
176900020924     C     REPDATIUTE    BEGSR
177000020924     C*
177100020924     C* INIZIALIZZA VARIABILI DI WRK
177200020924     C                   CLEAR                   TIBS34DS
177300020924     C                   CLEAR                   AZUTEDS
177400020924     C                   CLEAR                   DDATIUTE
177500020924     C*
177600020924     C     *DTAARA       DEFINE    §azute        azuteds
177700020924     C     *DTAARA       DEFINE    §datiute      ddatiute
177800020924     C                   IN(E)     *DTAARA
177900020924if  1C                   IF        %Error
178000020924     c                   EVAL      I34Tla = 'L'
178100020924     C                   CALL      'TIBS34R'
178200020924     C                   PARM                    Tibs34Ds
178300020924     C                   IN        *DTAARA
178400020924e   1C                   ENDIF
178500020924     C*
178600020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
178700020924     C                   MOVEL(P)  RSUT          DSFIRS           20
178800020924     C*
178900020924     C                   ENDSR
179000040422     C*------------------------------------------------------------------------*
179100040422     C* EXESGI - GESTIONE PARAMETRI PARTICOLARI X UTENTE STRATEGI
179200040422     C*------------------------------------------------------------------------*
179300040422     C     EXESGI        BEGSR
179400040422     C*
179500040422     C* Inizializzo il flag che indica l'autorizzazione speciale da attribuire al servizio
179600040422     C                   movel     *blanks       wABILSGI          1
179700040422     C*
179800040422     C* Innanzitutto verifico se il servizio è d tipo STRATEGI e quale autorizzazione speciale
179900040422     C* prevede
180000040422     C                   eval      KTBcod = 'ISV'
180100040422     C                   eval      KTBky1 = VIDISV
180200040422     C     KEYtbe        chain     tntbe01l
180300040422     C                   if        %found(tntbe01l)
180400040422     C                   eval      DSISV = tbeUNI
180500040422     C                   if        §ISVPROC = 'S' AND
180600040422     C                             §ISVSGI1 <> *blanks
180700040422     C                   eval      wABILSGI = §ISVSGI1
180800040422     C                   endif
180900040422     C                   endif
181000040422     C*
181100040422     C* Se x il servizio è prevista una particolare abilitazione => procedo
181200040422     C                   if        wABILSGI <> *blanks
181300040422     C* Costrisco la stringa d comando base x impostare i parametri adeguati sull'utente STRATEGI
181400040422     C                   eval      CMD = 'STRATEGI/CHGSGIUSR ' +
181500040422     C                                   VIDSUN + '  ' +
181600040422     C                                   'ALWWEB(' + api + '*YES' + api + ') '
181700040422     C*
181800040422     C* Gestisco i vari parametri diversi da abilitare a seconda del servizio
181900040422     C                   if        wABILSGI = 'U'                               * UPLOAD
182000040422     C                   eval      CMD = %trim(CMD) + ' ' +
182100040422     C                             'ALWRCVFILE(' + api + '*YES' + api + ') '
182200040422     C                   endif
182300040422     C                   if        wABILSGI = 'D'                               * DOWNLOAD
182400040422     C                   eval      CMD = %trim(CMD) + ' ' +
182500040422     C                                'ALWFILE(' + api + '*YES' + api + ') '
182600040422     C                   endif
182700040422     C                   if        wABILSGI = 'S'                               * OUTQ
182800040422     C                   eval      CMD = %trim(CMD) + ' ' +
182900040422     C                              ' ALWSPOOL(' + api + '*YES' + api + ') '
183000040422     C                   endif
183100040422     C*
183200040422     C* Esegue comando remoto su AS777
183300040422     C                   call(e)   'TIS730C'
183400040422     C                   parm                    cmd
183500040422     C*
183600040422     C                   endif
183700040422     C*
183800040422     C                   ENDSR
183900090610     C*------------------------------------------------------------------------*
184000090610     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
184100090610     C*------------------------------------------------------------------------*
184200090610     C     CHKSGI        BEGSR
184300090714      /FREE
184400090714               if UBUTISGI_Init = 0;
184500090714               endif;
184600090714      /END-FREE
184700090611     C*
184800090610      /FREE
184900090610               pInSUN = vidSUN;
185000090610               pInKSU = vidKSU;
185100090611               pInRGR = 'S';
185200090610
185300090610               if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
185400090610               endif;
185500090610
185600090610               if pOutEsito = '0';
185700090610                  if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
185800090610                  endif;
185900110503               else;
186000090611                  if UBUTISGI_EnableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
186100090611                  endif;
186200090611               endif;
186300090611      /END-FREE
186400090611     C
186500090923     C
186600090714      /FREE
186700090714               if UBUTISGI_Finalize = 0;
186800090714               endif;
186900090714      /END-FREE
187000090714     C*
187100090610     C                   ENDSR
187200101022
187300101022
187400101022
187500101022     c*------------------------------------------------------------------------*
187600101022     c* opnCONN - Apertura connessione
187700101022     c*------------------------------------------------------------------------*
187800101022     c     OPNCONN       BEGSR
187900101022      /FREE
188000101022
188100101022        if not *in40;
188200101022           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
188300101022              pInConn = pOutConn;
188400101022           else;
188500101022              *in40 = *on;
188600101022              s725_§21_ERRFLG = 'E';
188700101022              s725_§21_ERRMSG = 'Errore BartVAS: fatal error ';
188800101022           endif;
188900101022        endif;
189000101022
189100101022      /END-FREE
189200101022      *
189300101022     c                   ENDSR
189400101022     c*------------------------------------------------------------------------*
189500101022
189600101022
189700101022
189800101022     c*------------------------------------------------------------------------*
189900101022     c* clsCONN - Chiusura connessione
190000101022     c*------------------------------------------------------------------------*
190100101022     c     CLSCONN       BEGSR
190200101022      /FREE
190300101022
190400101022           if UBBVSQL01_Disconnect(pInConn) = 0;
190500101022           else;
190600101022              *in40 = *on;
190700101022           endif;
190800101022
190900101022      /END-FREE
191000101022      *
191100101022     c                   ENDSR
191200101022     c*------------------------------------------------------------------------*
191300101022
191400101022
191500101022
191600970526     C*------------------------------------------------------------------------*
191700970526     C* *INZSR - OPERAZIONI INIZIALI
191800970526     C*------------------------------------------------------------------------*
191900970526     C     *INZSR        BEGSR
192000970526     C*---
192100970526     C* RICEVIMENTO PARAMETRI
192200970526     C*---
192300970526     C     *ENTRY        PLIST
192400970612     C                   PARM                    KPJBA
192500001116     C                   MOVEL     KPJBU         tis720
192600020924     C*
192700020924     C* REPERISCE I DATI DELL'UTENTE
192800020924     C                   EXSR      REPDATIUTE
192900970526     C*
193000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
193100970526     C     XMSGPA        PLIST
193200970526     C                   PARM                    DSMSMS
193300970526     C                   PARM                    DSMSPG
193400970526     C                   PARM                    DSMSKE
193500970526     C                   PARM                    DSMSER
193600970526     C*
193700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
193800970526     C                   CALL      'XKEY'
193900970526     C                   PARM                    DSKEY
194000020212     C*
194100020212     C* CARICA LE TABELLE OCCORRENTI
194200020212     C                   EXSR      CARTAB
194300111216     C*
194400111216     C* Reperisco il sistema AS/400 corrente
194500111216     C                   EVAL      AS400_ok = *off
194600111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
194700111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
194800111216     C                   EVAL      AS400_ok = *on
194900111216     C                   ENDIF
195000101022     C*
195100101022     C* Apro la connessione al DB "BARTVAS"
195200111216     C                   IF        AS400_ok = *on
195300101022     C                   exsr      opnCONN
195400111216     C                   ENDIF
195500001116     C*---
195600001116     C* VARIABILI NON RIFERITE AL DATA BASE
195700001116     C*---
195800001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
195900001116     C     *LIKE         DEFINE    s20OP0        DEPOP0
196000001116     C     *LIKE         DEFINE    s20OP1        DEPOP1
196100001116     C     *LIKE         DEFINE    s20OP0        WOP0
196200001116     C     *LIKE         DEFINE    s20OP1        WOP1
196300001116     C*---
196400001116     C* VARIABILI RIFERITE AL DATA BASE
196500001116     C*---
196600001116     C     *LIKE         DEFINE    vssisv        KCEisv
196700001116     C     *LIKE         DEFINE    vsssun        KCEsun
196800001116     C     *LIKE         DEFINE    vssksu        KCEksu
196900001116     C     *LIKE         DEFINE    kuntle        KKUtle
197000001116     C     *LIKE         DEFINE    kunazp        KKUazp
197100001116     C     *LIKE         DEFINE    kuncop        KKUcop
197200020211     C     *LIKE         DEFINE    kuncof        KKUcof
197300020211     C     *LIKE         DEFINE    kundde        KKUdde
197400001116     C     *LIKE         DEFINE    tbecod        KTBcod
197500001116     C     *LIKE         DEFINE    tbeke1        KTBky1
197600010605     C     *LIKE         DEFINE    virtip        KCEtip
197700010605     C     *LIKE         DEFINE    virksc        KCEksc
197800010605     C     *LIKE         DEFINE    virdti        KCEdti
197900001116     C*---
198000001116     C* VARIABILI DI CONTROLLO
198100001116     C*---
198200001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
198300001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
198400001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
198500001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
198600010608     C                   MOVEL     '0'           ERRL01            1            *ERRORE (x TIVIR)
198700001116     C*---
198800001116     C* CHIAVI DI LETTURA
198900001116     C*---
199000001116     C* LETTURA tivss01L - COMPLETA
199100001116     C     KEYvss        KLIST
199200001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
199300001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
199400100217     C* LETTURA tivss02L - PARZIALE
199500001116     C     KE2vss        KLIST
199600001116     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
199700001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
199800100217     C* LETTURA tivss02L - COMPLETA
199900100217     C     KEYvss02_C    KLIST
200000100217     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
200100100217     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
200200100217     C                   KFLD                    KCEsun                         *STRATEGI USER NBR
200300001116     C* LETTURA tikun01L
200400001116     C     KEYkun        KLIST
200500001116     C                   KFLD                    KKUtle                         *TIPO LEGAME
200600001116     C                   KFLD                    KKUazp                         *AZIENDA PADRE
200700001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
200800001116     C* LETTURA tikun03L
200900001116     C     KE3kun        KLIST
201000001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
201100020211     C* LETTURA tikun06L
201200020211     C     KE6kun        KLIST
201300020211     C                   KFLD                    KKUcof                         *CODICE CLI FIGLIO
201400020212     C                   KFLD                    KKUtle                         *TIPO LEGAME
201500001116     C* LETTURA tntbe01l
201600001116     C     KEYtbe        KLIST
201700001116     C                   KFLD                    KTBcod                         *CODICE TABELLA
201800001116     C                   KFLD                    KTBky1                         *CHIAVE 1 TABELLA
201900010605     C* LETTURA tivir03L - COMPLETA
202000010605     C     KEYvi2        KLIST
202100010605     C                   KFLD                    KCEtip                         *TIPO FILE
202200010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
202300010605     C                   KFLD                    KCEdti                         *DATA INIZIO ABIL.
202400010605     C* LETTURA tivir03L - INCOMPLETA
202500010605     C     KEYvir        KLIST
202600010605     C                   KFLD                    KCEtip                         *TIPO FILE
202700010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
202800970526     C*
202900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
203000991104     C                   MOVEL     'TIS725R'     DSMSPG
203100991104     C                   MOVEL     'TIS725R'     MSGQUE
203200970526     C                   MOVEL     *BLANKS       MSGKEY
203300970526     C                   MOVEL     *BLANKS       DSMSMS
203400970528     C*
203500970528     C* CALCOLA LA DATA CORRENTE
203600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
203700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
203800970528     C                   Z-ADD     WN8           G08DAT
203900051013     C                   Z-ADD     WN8           DATCORINV         8 0
204000970528     C                   Z-ADD     *ZEROS        G08INV
204100970528     C                   MOVEL     '0'           G08ERR
204200970528     C                   CALL      'XSRDA8'
204300970528     C                   PARM                    WLBDA8
204400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
204500970526     C*
204600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
204700991104     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
204800991104     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
204900051013     C*
205000051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
205100051013     C                   IF        DEPOP1 = 'F06' OR
205200051013     C                             DEPOP1 = 'O03' OR
205300051013     C                             DEPOP1 = 'O07'
205400051013     C                   EVAL      VIDDDE = DATCORINV
205500060726     C                   EVAL      VIDDSC = 31122039
205600051013     C                   ENDIF
205700101022     C                   IF        DEPOP1 = 'F06' OR
205800101022     C                             DEPOP1 = 'O03'
205900101022     C                   SETOFF                                       81
206000101022     C                   ELSE
206100101022     C                   SETON                                        81
206200101022     C                   ENDIF
206300970526     C*
206400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
206500970526     C                   EXSR      IMPFUN
206600970526     C*
206700000131     C* ESEGUE SUBITO "DECODI" SE ENTRATI IN VISUALIZZAZIONE O MODIFICA
206800000915     C*    DEPOP1        IFEQ      'O05'
206900000915     C*    DEPOP1        OREQ      'O02'
207000000915     C*                  EXSR      DECODI
207100000915     C*                  ENDIF
207200000131     C*
207300970526     C                   ENDSR
207400970526     C*------------------------------------------------------------------------*
207500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
207600970526     C*------------------------------------------------------------------------*
207700970526     C     FUNGES        BEGSR
207800970526     C*
207900970526     C* PULISCO CAMPI DI LAVORO
208000970526     C                   Z-ADD     0             W                 5 0
208100970526     C*
208200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
208300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
208400970526     C     1             DO        24            I
208500970526     C                   MOVEL     FUD(I)        FUDRIG
208600970526     C*
208700970526     C* PULIZIA FUNZIONI PRECEDENTI
208800970526     C                   MOVEL     *BLANKS       FUV(I)
208900970526     C*
209000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
209100970526     C* (SALTA F24)
209200970526     C     FUDATT        IFEQ      'S'
209300970526     C     FUDFUN        ANDNE     '24'
209400970526     C                   ADD       1             W
209500970526     C                   MOVEL     FUDDES        FUV(W)
209600970526     C                   END
209700970526     C                   END                                                    DO WHILE
209800970526     C*
209900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
210000970526     C* ALTRIMENTI DISATTIVO F24
210100970526     C     1             DO        24            I
210200970526     C                   MOVEL     FUD(I)        FUDRIG
210300970526     C*
210400970526     C     FUDFUN        IFEQ      '24'
210500970526     C     W             IFGT      6
210600970526     C                   MOVEL     'S'           FUDATT
210700970526     C                   ELSE
210800970526     C                   MOVEL     *BLANKS       FUDATT
210900970526     C                   END                                                    W GT 8
211000970526     C                   MOVEL     FUDRIG        FUD(I)
211100970526     C                   END
211200970526     C                   END                                                    DO WHILE
211300970526     C*
211400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
211500970526     C     W             DIV       6             I
211600970526     C                   MVR                     Z
211700970526     C*
211800970526     C     Z             IFNE      0
211900970526     C     I             ADD       1             Z
212000970526     C                   ELSE
212100970526     C                   Z-ADD     I             Z                 5 0
212200970526     C                   END
212300970526     C*
212400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
212500970526     C                   ADD       1             FUNGRU
212600970526     C     FUNGRU        IFGT      Z
212700970526     C     FUNGRU        ORLT      1
212800970526     C                   Z-ADD     1             FUNGRU
212900970526     C                   END
213000970526     C*
213100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
213200970526     C     FUNGRU        IFEQ      1
213300970526     C                   MOVEL     FUV(1)        FUN001
213400970526     C                   MOVEL     FUV(2)        FUN002
213500970526     C                   MOVEL     FUV(3)        FUN003
213600970526     C                   MOVEL     FUV(4)        FUN004
213700970526     C                   MOVEL     FUV(5)        FUN005
213800970526     C                   MOVEL     FUV(6)        FUN006
213900970526     C                   END
214000970526     C*
214100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
214200970526     C     FUNGRU        IFEQ      2
214300970526     C                   MOVEL     FUV(7)        FUN001
214400970526     C                   MOVEL     FUV(8)        FUN002
214500970526     C                   MOVEL     FUV(9)        FUN003
214600970526     C                   MOVEL     FUV(10)       FUN004
214700970526     C                   MOVEL     FUV(11)       FUN005
214800970526     C                   MOVEL     FUV(12)       FUN006
214900970526     C                   END
215000970526     C*
215100970526     C* TERZO GRUPPO DI 6 FUNZIONI
215200970526     C     FUNGRU        IFEQ      3
215300970526     C                   MOVEL     FUV(13)       FUN001
215400970526     C                   MOVEL     FUV(14)       FUN002
215500970526     C                   MOVEL     FUV(15)       FUN003
215600970526     C                   MOVEL     FUV(16)       FUN004
215700970526     C                   MOVEL     FUV(17)       FUN005
215800970526     C                   MOVEL     FUV(18)       FUN006
215900970526     C                   END
216000970526     C*
216100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
216200970526     C     FUNGRU        IFEQ      4
216300970526     C                   MOVEL     FUV(19)       FUN001
216400970526     C                   MOVEL     FUV(20)       FUN002
216500970526     C                   MOVEL     FUV(21)       FUN003
216600970526     C                   MOVEL     FUV(22)       FUN004
216700970526     C                   MOVEL     *BLANKS       FUN005
216800970526     C                   MOVEL     *BLANKS       FUN006
216900970526     C                   END
217000970526     C*
217100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
217200970526     C                   MOVEL     *BLANKS       FUN007
217300970526     C                   MOVEL     *BLANKS       FUN008
217400970526     C*
217500970526     C     1             DO        24            I
217600970526     C                   MOVEL     FUD(I)        FUDRIG
217700970526     C     FUDFUN        IFEQ      '24'
217800970526     C     FUDATT        ANDEQ     'S'
217900970526     C                   MOVEL     FUDDES        FUN008
218000970526     C                   END
218100970526     C                   END                                                    DO
218200970526     C*
218300970526     C                   ENDSR
218400970526     C*------------------------------------------------------------------------*
218500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
218600970526     C*------------------------------------------------------------------------*
218700970526     C     FUNCON        BEGSR
218800970526     C*
218900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
219000970526     C                   EXSR      FUNVAR
219100970526     C*
219200970526     C* PULISCO CAMPI DI LAVORO
219300970526     C                   Z-ADD     0             I                 5 0
219400970526     C                   Z-ADD     0             W                 5 0
219500970526     C*
219600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
219700970526     C     FUNKEY        IFNE      *BLANKS
219800970526     C     1             DO        24            I
219900970526     C                   MOVEL     FUD(I)        FUDRIG
220000970526     C*
220100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
220200970526     C     FUNKEY        IFEQ      FUDFUN
220300970526     C     FUDATT        ANDEQ     'S'
220400970526     C                   Z-ADD     I             W
220500970526     C                   END
220600970526     C                   END                                                    DO WHILE
220700970526     C*
220800970526     C* FUNZIONE NON ATTIVA (ERRORE)
220900970526     C     W             IFEQ      0
221000970526     C*
221100970526     C* EMISSIONE MESSAGGIO DI ERRORE
221200970526     C                   MOVEL     MSG(1)        DSMSMS
221300970526     C*
221400970526     C                   END                                                    W=0
221500970526     C                   END                                                    FUNKEY NE *BLANKS
221600970526     C*
221700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
221800970526     C     W             IFNE      0
221900970526     C     FUNKEY        OREQ      *BLANKS
222000970526     C     FUNKEY        IFEQ      *BLANKS
222100970526     C                   EXSR      ENTGES
222200970526     C                   END
222300970526     C     FUNKEY        IFEQ      '03'
222400970526     C                   EXSR      F03GES
222500970526     C                   END
222600970526     C     FUNKEY        IFEQ      '06'
222700970526     C                   EXSR      F06GES
222800970526     C                   END
222900170915     C     FUNKEY        IFEQ      '08'
223000170915     C                   EXSR      F08GES
223100170915     C                   END
223200970526     C     FUNKEY        IFEQ      '12'
223300970526     C                   EXSR      F12GES
223400970526     C                   END
223500100914     C     FUNKEY        IFEQ      '18'
223600100914     C                   EXSR      F18GES
223700100914     C                   END
223800970526     C     FUNKEY        IFEQ      '24'
223900970526     C                   EXSR      F24GES
224000970526     C                   END
224100970526     C                   END
224200970526     C*
224300970526     C                   ENDSR
224400970526     C*------------------------------------------------------------------------*
224500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
224600970526     C*------------------------------------------------------------------------*
224700970526     C     FUNVAR        BEGSR
224800970526     C*
224900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
225000970526     C*
225100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
225200970526     C     KEY           IFEQ      ENTER
225300970526     C                   MOVEL     *BLANKS       FUNKEY
225400970526     C                   END
225500970526     C     KEY           IFEQ      F1
225600970526     C                   MOVEL     '01'          FUNKEY
225700970526     C                   END
225800970526     C     KEY           IFEQ      F2
225900970526     C                   MOVEL     '02'          FUNKEY
226000970526     C                   END
226100970526     C     KEY           IFEQ      F3
226200970526     C                   MOVEL     '03'          FUNKEY
226300970526     C                   END
226400970526     C     KEY           IFEQ      F4
226500970526     C                   MOVEL     '04'          FUNKEY
226600970526     C                   END
226700970526     C     KEY           IFEQ      F5
226800970526     C                   MOVEL     '05'          FUNKEY
226900970526     C                   END
227000970526     C     KEY           IFEQ      F6
227100970526     C                   MOVEL     '06'          FUNKEY
227200970526     C                   END
227300970526     C     KEY           IFEQ      F7
227400970526     C                   MOVEL     '07'          FUNKEY
227500970526     C                   END
227600970526     C     KEY           IFEQ      F8
227700970526     C                   MOVEL     '08'          FUNKEY
227800970526     C                   END
227900970526     C     KEY           IFEQ      F9
228000970526     C                   MOVEL     '09'          FUNKEY
228100970526     C                   END
228200970526     C     KEY           IFEQ      F10
228300970526     C                   MOVEL     '10'          FUNKEY
228400970526     C                   END
228500970526     C     KEY           IFEQ      F11
228600970526     C                   MOVEL     '11'          FUNKEY
228700970526     C                   END
228800970526     C     KEY           IFEQ      F12
228900970526     C                   MOVEL     '12'          FUNKEY
229000970526     C                   END
229100970526     C     KEY           IFEQ      F13
229200970526     C                   MOVEL     '13'          FUNKEY
229300970526     C                   END
229400970526     C     KEY           IFEQ      F14
229500970526     C                   MOVEL     '14'          FUNKEY
229600970526     C                   END
229700970526     C     KEY           IFEQ      F15
229800970526     C                   MOVEL     '15'          FUNKEY
229900970526     C                   END
230000970526     C     KEY           IFEQ      F16
230100970526     C                   MOVEL     '16'          FUNKEY
230200970526     C                   END
230300970526     C     KEY           IFEQ      F17
230400970526     C                   MOVEL     '17'          FUNKEY
230500970526     C                   END
230600970526     C     KEY           IFEQ      F18
230700970526     C                   MOVEL     '18'          FUNKEY
230800970526     C                   END
230900970526     C     KEY           IFEQ      F19
231000970526     C                   MOVEL     '19'          FUNKEY
231100970526     C                   END
231200970526     C     KEY           IFEQ      F20
231300970526     C                   MOVEL     '20'          FUNKEY
231400970526     C                   END
231500970526     C     KEY           IFEQ      F21
231600970526     C                   MOVEL     '21'          FUNKEY
231700970526     C                   END
231800970526     C     KEY           IFEQ      F22
231900970526     C                   MOVEL     '22'          FUNKEY
232000970526     C                   END
232100970526     C     KEY           IFEQ      F23
232200970526     C                   MOVEL     '23'          FUNKEY
232300970526     C                   END
232400970526     C     KEY           IFEQ      F24
232500970526     C                   MOVEL     '24'          FUNKEY
232600970526     C                   END
232700970526     C     KEY           IFEQ      ROLLUP
232800970526     C                   MOVEL     'RU'          FUNKEY
232900970526     C                   END
233000970526     C     KEY           IFEQ      HELP
233100970526     C                   MOVEL     'HP'          FUNKEY
233200970526     C                   END
233300970526     C                   ENDSR
233400970526     C*------------------------------------------------------------------------*
233500970526     C* NOTE COSTRUZIONE SCHIERE
233600970526     C*------------------------------------------------------------------------*
233700970526     C*
233800970526     C* -MSG-MESSAGGI
233900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
234000970526     C* (NON TOCCARE MSG,1 E MSG,2)
234100970526     C*
234200970526     C* -CMD-COMANDI
234300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
234400970526     C*
234500970526     C* -OPD-OPZIONI
234600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
234700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
234800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
234900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
235000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
235100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
235200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
235300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
235400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
235500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
235600970526     C*
235700970526     C* -FUD-FUNZIONI
235800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
235900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
236000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
236100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
236200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
236300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
236400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
236500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
236600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
236700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
236800970526     O*------------------------------------------------------------------------*
236900970526** DEC - DECODIFICHE
237000970526    MODIFICA                                                      1
237100970526  ANNULLAMENTO                                                    2
237200970526VISUALIZZAZIONE                                                   3
237300970526   RIPRISTINO                                                     4
237400970526  INSERIMENTO                                                     5
237500991105     COPIA                                                        6
237600000215** DES - DESCRIZIONI
237700000215ATTENZIONE: non esistono figli legati                             1
237800000215ATTENZIONE: esistono figli legati                                 2
237900020211                                                                  3
238000000215                                                                  4
238100000215                                                                  5
238200970526** MSG - MESSAGGI
238300991104TIS725R- Tasto funzionale non gestito                                          1
238400991104TIS725R- Inserimento effettuato                                                2
238500991104TIS725R- Modifica effettuata                                                   3
238600991104TIS725R- Annullamento effettuato                                               4
238700991104TIS725R- Ripristino effettuato                                                 5
238800991104TIS725R- Richiesta funzione per record inesistente                             6
238900991104TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
239000991104TIS725R- Richiesta funzione per record esistente                               8
239100991104TIS725R- Codice tipo servizio non presente in tabella                          9
239200000322TIS725R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
239300000317TIS725R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
239400091117TIS725R- Tipo legame inesistente o non utilizzabile                            12
239500991104TIS725R- Codice padre errato                                                   13
239600991104TIS725R- Data errata                                                           14
239700991104TIS725R- La data scadenza deve essere maggiore della decorrenza                15
239800991104TIS725R- Il record non può scadere in un giorno già trascorso                  16
239900991104TIS725R- Il record immesso si sovrappone ad uno già in decorrenza              17
240000991104TIS725R- Codice figlio errato                                                  18
240100000705TIS725R- Codice padre obbligatorio                                             19
240200991104TIS725R- Il codice figlio non può essere uguale al codice padre                20
240300000719TIS725R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
240400000807TIS725R- Tipo legame non autorizzato.                                          22
240500000807TIS725R- Filiale non abilitata alla gestione dei clienti scelti.               23
240600000913TIS725R- Servizio DL già presente nel periodo per il cliente scelto.           24
240700010605TIS725R- Nel periodo esistono già abilitazioni per il cliente al file TO.      25
240800010605TIS725R- Abilitazione per il cliente al file TO già esistente.                 26
240900020211TIS725R- Cliente già presente nei legami come figlio.                          27
241000020214TIS725R- Per il servizio indicato non è consentito specificare un legame.      28
241100020802TIS725R- Il codice Strategi è incompatibile con il servizio richiesto.         29
241200090804TIS725R- Campo valido solamente per servizi 'TT'                               30
241300100217TIS725R- E' necessaria una identica abilitazione al servizio 'TT'              31
241400100913TIS725R- Password necessaria x attivazione servizio richiesto.                 32
241500100913TIS725R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    33
241600100913TIS725R- Lunghezza minima password: 8 caratteri                                34
241700100914TIS725R- Tasto funzione non consentito per servizi diversi da 'IE'             35
241800101027TIS725R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       36
241900120327TIS725R- Cliente annullato o bloccato, impossibile abilitare EasyWeb           37
242000101027TIS725R-                                                                       38
242100101027TIS725R-                                                                       39
242200101027TIS725R-                                                                       40
242300970526** FUD - FUNZIONI
242400970526                   -        (S/N)=N                        funz.01¬
242500970526                   -        (S/N)=N                        funz.02¬
242600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
242700970526                   -        (S/N)=N                        funz.04¬
242800970526                   -        (S/N)=N                        funz.05¬
242900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
243000970527                   -        (S/N)=N                        funz.07¬
243100170915F08=Anagr. varie   -        (S/N)=S                        funz.08¬
243200170915                   -        (S/N)=N                        funz.09¬
243300970527                   -        (S/N)=N                        funz.10¬
243400970526                   -        (S/N)=N                        funz.11¬
243500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
243600970526                   -        (S/N)=N                        funz.13¬
243700970526                   -        (S/N)=N                        funz.14¬
243800970526                   -        (S/N)=N                        funz.15¬
243900970526                   -        (S/N)=N                        funz.16¬
244000970526                   -        (S/N)=N                        funz.17¬
244100100914F18=Conf.EasySped  -        (S/N)=S                        funz.18¬
244200970526                   -        (S/N)=N                        funz.19¬
244300970526                   -        (S/N)=N                        funz.20¬
244400970526                   -        (S/N)=N                        funz.21¬
244500970526                   -        (S/N)=N                        funz.22¬
244600970526                   -        (S/N)=N                        funz.23¬
244700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
