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
009800170111     D tis732ds      E DS
009900970526     D*------------
010000970526     D* ARCHITETTURA
010100970526     D*------------
010200970526     D KPJBA         E DS
010300100406     D*------------
010400100406     D* DS DEFINIZIONE TABELLA '3EW'
010500100406     D*------------
010600100406     D D3EW          E DS                  qualified
010700000807     D*------------
010800000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
010900000807     D ksc             s              7  0 dim(30)
011000000807     D ksa             s              4    dim(30)
011100020212     D isvkun          s              2    dim(100)
011200050118     D isvtt           s              2    dim(100)
011300100913     D isvftp          s              2    dim(100)
011400100217     D isvKEYS         s              2    dim(100)
011500100217     D isvCTRL         s              4    dim(100)
011600101006     D isvPGMAFT       s              3    dim(100)
011700000807     D*------------------
011800000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
011900000807     D*------------------
012000000807     DDSXC           e DS                  extname(XCLIRDS)
012100000807     D*---
012200000807     D                 DS
012300000807     D dsksn                   1      4p 0
012400000807     D dsksa                   1      4
012500000807     DBS69DS         e ds                  extname(TIBS69DS)
012600000807     DACODS          e ds                  extname(cnaco00f)
012700000807     DINDDS          e ds                  extname(cnind00f)
012800000807     DCLPDS          e ds                  extname(cnclp00f)
012900000807     DCLSDS          e ds                  extname(fncls00f)
013000000807     D*---
013100000807     D* VARIABILI NON RIFERITE AL DATABASE
013200000807     D*---
013300000807     D $esikun         S              1                                         *Esistenza KUN
013400000807     D chk             S              1    inz('1')
013500000807     D giro            S              1  0 inz(*zeros)
013600000807     D tipprf          s              3    inz(*blanks)
013700040422     D cmd             s            500
013800040422     D api             s              1    inz('''')
013900090611     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
014000091027     D sktle           S              2    DIM(20)
014100091027     D jtle            S              5  0                                      *indici
014200100914     D OPERAZ          S              3A   inz
014300100914     D savKPJBU        S                   like(KPJBU)
014400100914     D ds_param        S           1024    inz
014500100913     D*
014600100913     D                 ds
014700100913     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
014800100913     D                                     456789abcdefghijklmnopqrstuvwxyz')
014900100913     D  valAry                        1A   DIM(63)
015000100913     D                                     OVERLAY(val)
015100000807     D*------------
015200000807     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
015300000807     D*------------
015400000807     D                 DS
015500000807     D  FILCLI                 5      7  0
015600000807     D  STRATECLI              1     11
015700020924     D*-------------------
015800020924     D* DS REPERIMENTO DATI UTENTE
015900020924     D*-------------------
016000020924     D TIBS34DS      E DS                                                       *Profili utente
016100020924     D DDATIUTE      E DS                                                       *Dati utente
016200020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016300111216     D*------------------
016400111216     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
016500111216     D*------------------
016600111216     D currSysNeta     s              8A   inz('*NULL')
016700111216     D AS400_ok        s              1A
016800111216     D  PiOutSysNeta   S              8A
016900090610     D*-------------------
017000090610     D* DEFINIZIONI PROCEDURE ESTERNE
017100090610     D*-------------------
017200090610     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
017300090610     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
017400101022     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
017500101022     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
017600100406     D/COPY GAITRASRC/SRCPROTOPR,UB§3EWUPD
017700110419     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
017800110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
017900111216     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
018000970526     C*------------------------------------------------------------------------*
018100970526     C* MAIN LINES
018200970526     C*------------------------------------------------------------------------*
018300970526     C*
018400970526     C* NON CI SONO ERRORI IN ENTRATA
018500000915IF  1C*    ERRL00        IFEQ      '0'
018600970526     C*
018700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
018800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
018900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
019000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
019100970526     C*
019200970526     C* EMETTE FORMATO VIDEO
019300970526     C                   EXSR      BUTFMR
019400970526     C*
019500970526     C* GESTIONE TASTI FUNZIONALI
019600970526     C                   EXSR      FUNCON
019700970526E   2C                   ENDDO
019800970526     C*
019900970526     C* CI SONO ERRORI IN ENTRATA
020000000915X   1C*                  ELSE
020100000915     C*                  MOVEL     '1'           s20err
020200000915     C*                  MOVEL     DSMSMS        s20msg
020300000915E   1C*                  ENDIF
020400001116     C*
020500970526     C* FINE PGM
020600001116     C                   EXSR      FINSR
020700001116     C*
020800100914     C                   MOVEL     tis720        KPJBU
020900101022     C*
021000101022     C* Chiudo la connessione
021100111216     C                   IF        AS400_ok = *on
021200101022     C                   exsr      clsCONN
021300111216     C                   ENDIF
021400101022     C*
021500970526     C                   SETON                                        LR
021600100913     C*------------------------------------------------------------------------*
021700100913     C* RESETIN - RESET INDIRCATORI
021800100913     C*------------------------------------------------------------------------*
021900100913     C     RESETIN       BEGSR
022000100913     C*
022100100913     C* REIMPOSTA INDICATORI DI ERRORE
022200100913     C                   SETOFF                                         5859
022300100913     C                   SETOFF                                       606162
022400100913     C                   SETOFF                                       636465
022500100913     C                   SETOFF                                       666768
022600100913     C                   SETOFF                                       697071
022700100913     C                   SETOFF                                       727374
022800100913     C                   SETOFF                                       757677
022900100913     C                   SETOFF                                       787980
023000100913     C*
023100100913     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     BUTFMR        BEGSR
023600970526     C*
023700970526     C* EMETTE LA TESTATA
023800991104     C                   WRITE     S725D01
023900970526     C*
024000970526     C* SPEDISCE EVENTUALE MESSAGGIO
024100970526     C                   EXSR      SNDMSG
024200970526     C*
024300970526     C* EMETTE LE FUNZIONI
024400991104     C                   WRITE     S725D02
024500970526     C*
024600970526     C* EMETTE IL VIDEO
024700991104     C                   EXFMT     S725D03
024800970526     C*
024900970526     C* REIMPOSTA FLAG DI ERRORE
025000970526     C                   MOVEL     '0'           ERRL00
025100100913     C*
025200100913     C* RESETTA INDICATORI DI CONTROLLO
025300100913     C                   EXSR      RESETIN
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
025800970526     C*------------------------------------------------------------------------*
025900970526     C     SNDMSG        BEGSR
026000970526     C*
026100970526     C* PULISCE IL SFILE MESSAGGI
026200970526     C                   MOVEL     '1'           *IN40
026300991104     C                   WRITE     S725DMC
026400970526     C                   MOVEL     '0'           *IN40
026500970526     C*
026600970526     C* VISUALIZZA IL MESSAGGIO
026700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
026800970526     C                   CALL      'XMSG'        XMSGPA
026900970526IF  1C     DSMSER        IFEQ      *ZEROS
027000970526     C                   MOVEL     DSMSKE        MSGKEY
027100970526E   1C                   ENDIF
027200970526     C                   Z-ADD     1             MSGULT
027300991104     C                   WRITE     S725DMS
027400991104     C                   WRITE     S725DMC
027500970526     C*
027600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
027700970526     C                   MOVEL     *BLANKS       DSMSMS
027800970526     C*
027900970526     C                   ENDSR
028000970526     C*------------------------------------------------------------------------*
028100970526     C* ENTGES - GESTIONE ENTER -> INVIO
028200970526     C*------------------------------------------------------------------------*
028300970526     C     ENTGES        BEGSR
028400970526     C*
028500970526     C* CONTROLLA I CAMPI IMMESSI
028600970526     C                   EXSR      DECODI
028700970526     C*
028800970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
028900970526IF  1C     DEPOP1        IFEQ      'O05'
029000970526     C                   MOVEL     '1'           FINL00
029100970526E   1C                   ENDIF
029200970526     C*
029300970526     C                   ENDSR
029400970526     C*------------------------------------------------------------------------*
029500970526     C* F03GES - GESTIONE F03 -> FINE
029600970526     C*------------------------------------------------------------------------*
029700970526     C     F03GES        BEGSR
029800970526     C*
029900970526     C* IMPOSTA "PREMUTO F03"
030000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
030100991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
030200970526     C*
030300970526     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
030600970526     C*------------------------------------------------------------------------*
030700970526     C     F06GES        BEGSR
030800970526     C*
030900970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
031000970526IF  1C     DEPOP1        IFEQ      'F06'
031100100914     C                   EVAL      OPERAZ = 'INS'
031200970526     C                   EXSR      INSERI
031300970526E   1C                   ENDIF
031400970526IF  1C     DEPOP1        IFEQ      'O02'
031500100914     C                   EVAL      OPERAZ = 'UPD'
031600970526     C                   EXSR      MODIFI
031700090610     C                   EXSR      CHKSGI
031800970526E   1C                   ENDIF
031900991014IF  1C     DEPOP1        IFEQ      'O03'
032000100914     C                   EVAL      OPERAZ = 'INS'
032100000719     C                   EXSR      COPIA
032200991014E   1C                   ENDIF
032300021024IF  1C     DEPOP1        IFEQ      'O04'
032400100914     C                   EVAL      OPERAZ = 'DEL'
032500021024     C                   EXSR      ANNULL
032600090610     C                   EXSR      CHKSGI
032700021024E   1C                   ENDIF
032800970526IF  1C     DEPOP1        IFEQ      'O07'
032900100914     C                   EVAL      OPERAZ = 'UPD'
033000970526     C                   EXSR      RIPRIS
033100090610     C                   EXSR      CHKSGI
033200970526E   1C                   ENDIF
033300101012     C*
033400101012     C                   ENDSR
033500101012     C*------------------------------------------------------------------------*
033600101012     C* EXEPOST - GESTIONE "POST AGGIORNAMNETO"
033700101012     C*------------------------------------------------------------------------*
033800101012     C     EXEPOST       BEGSR
033900101012     C*
034000100217     C*
034100110210     C* Se no errore => gestione eventuale *PGM POST-AGGIORNAMENTO (solo x "INSERIMENTI")
034200110214 xxx C***                if        ERRL00 <> '1' and OPERAZ = 'INS'
034300110214 xxx C                   if        ERRL00 <> '1'
034400101005     C                   seton                                        33
034500101005     C*
034600101006     C                   movel     *blanks       wPGMAFTER         3
034700100217     C                   z-add     1             jISV              3 0
034800100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
034900100217     C                   if        *in22 = *on
035000100217     C                   if        isvPGMAFT(jISV) <> *blanks
035100100217     C                   eval      wPGMAFTER = isvPGMAFT(jISV)
035200100914     C                   eval      savKPJBU = KPJBU
035300100914     C*
035400101004     C                   select
035500110210     C* - 002
035600110210     C                   when      wPGMAFTER = '002'
035700100914     C                   clear                   TIS725DS21
035800100914     C                   select
035900110210     C                   when      FUNKEY = '06'
036000101022 xxx C***                setoff                                       33
036100101005     C                   eval      s725_§21_FILE   = 'VSS'
036200100914     C                   when      FUNKEY = '18'
036300110210 xxx C***                setoff                                       33
036400100914     C                   eval      s725_§21_FILE   = 'ESY'
036500100914     C                   endsl
036600100914     C                   eval      s725_§21_OPERAZ = OPERAZ
036700100914     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
036800100914     C                   eval      s725_§21_DESCR  = VDDKSU
036900100914     C                   eval      s725_§21_PASSW  = VIDPWDFTP
037000100914     C                   eval      ds_param = TIS725DS21
037100101006     C* - 003
037200110210     C                   when      wPGMAFTER = '003'
037300101005     C                   clear                   TIS725DS21
037400101022 xxx C***                setoff                                       33
037500101005     C                   eval      s725_§21_FILE   = 'VSS'
037600101005     C                   eval      s725_§21_OPERAZ = OPERAZ
037700101005     C                   eval      s725_§21_UTENTE = %subst(vssKSU:2:7)
037800101005     C                   eval      s725_§21_DESCR  = VDDKSU
037900101005     C                   eval      s725_§21_PASSW  = VIDPWDFTP
038000101005     C                   eval      ds_param = TIS725DS21
038100170111     C* - 004
038200170111     C                   when      wPGMAFTER = '004'
038300170111     C                   clear                   TIS732DS
038400170111 xxx C***                setoff                                       33
038500170112     C                   eval      S32OP0 = DEPOP0
038600170112     C                   eval      S32OP1 = DEPOP1
038700170111     C                   eval      S32SUN = VIDSUN
038800170111     C                   eval      S32ISV = VIDISV
038900170111     C                   eval      S32TIP = 'VAS'
039000170111     C                   eval      S32KSU = VIDKSU
039100170111     C                   eval      ds_param = TIS732DS
039200101004     C*
039300101004     C                   endsl
039400100914     C*
039500101005     C   33              call      'TIS725R1'
039600100914     C                   parm                    KPJBA
039700100217     C                   parm                    wPGMAFTER
039800100217     C                   parm                    tivssds
039900100914     C                   parm                    ds_param
040000100914     C*
040100101006     C                   if        wPGMAFTER = '002' OR
040200101006     C                             wPGMAFTER = '003'
040300100914     C                   eval      TIS725DS21 = ds_param
040400100914     C                   if        s725_§21_ERRFLG <> *blanks
040500100914     C                   eval      s20MSG = s725_§21_ERRMSG
040600100914     C                   eval      DSMSMS = s725_§21_ERRMSG
040700100914     C                   endif
040800100914     C                   if        s725_§21_ERRFLG <> *blanks
040900100914     C                   MOVEL     '1'           ERRL00
041000100914     C                   endif
041100100914     C                   endif
041200100914     C*
041300100914     C                   eval      KPJBU = savKPJBU
041400100217     C                   endif
041500100217     C                   endif
041600101005     C*
041700101005     C                   endif
041800970526     C*
041900970526     C                   ENDSR
042000970526     C*------------------------------------------------------------------------*
042100970526     C* F12GES - GESTIONE F12 -> RITORNO
042200970526     C*------------------------------------------------------------------------*
042300970526     C     F12GES        BEGSR
042400970526     C*
042500970526     C* IMPOSTA "PREMUTO F12"
042600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
042700991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
042800970526     C*
042900970526     C                   ENDSR
043000100914     C*------------------------------------------------------------------------*
043100100914     C* F18GES - GESTIONE F18 -> AGGIORNAMENTO EASYSPED
043200100914     C*------------------------------------------------------------------------*
043300100914     C     F18GES        BEGSR
043400100914     C*
043500100914     C                   EXSR      F06GES
043600110419     C*
043700110419     C* Forzo l'elaborazione lato server
043800110419     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
043900110419     C                                           '0ADMIN':'#U')
044000100914     C*
044100100914     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
044400970526     C*------------------------------------------------------------------------*
044500970526     C     F24GES        BEGSR
044600970526     C*
044700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
044800970526     C*
044900970526     C                   ENDSR
045000970526     C*------------------------------------------------------------------------*
045100010605     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
045200970526     C*------------------------------------------------------------------------*
045300970526     C     INSERI        BEGSR
045400970526     C*
045500970526     C* CONTROLLA I CAMPI IMMESSI
045600970526     C                   EXSR      DECODI
045700000731     C                   IF        chk = '1' and giro = 1
045800000719     C                   MOVEL     '1'           ERRL00
045900000719     C                   ENDIF
046000970526     C*
046100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
046200010605     C     ERRL00        IFEQ      '0'                                          *ANCORA NO ERRORI
046300000719     C                   MOVE      VIDsun        vsssun
046400010605     C                   MOVE      VIDisv        vssisv
046500991104     C                   EXSR      IMPvss                                       *ALTRI CAMPI
046600991104     C                   Z-ADD     *ZEROS        vssDTR                         *DATA TRASMISSIONE
046700991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
046800991104     C                   WRITE     tivss000
046900000719     C                   Z-ADD     *zeros        giro
047000100914     C                   MOVEL     MSG(2)        s20MSG                         *MESSAGGIO
047100100914     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
047200010605     C*
047300010605     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
047400010608     C                   IF        ERRL01 = '0'  and
047500010608     C                             VIDisv = 'TT'
047600010626 ORM C*                  EXSR      IMPvir
047700010626 ORM C*                  WRITE     tivir000
047800010605     C                   ENDIF
047900040422     C*
048000040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
048100040422     C                   EXSR      EXESGI
048200101012     C*
048300101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
048400101012     C                   EXSR      EXEPOST
048500970602     C*
048600970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
048700970602     C                   EXSR      AZZVID
048800010605     C                   ENDIF
048900970526     C*
049000970526     C                   ENDSR
049100970526     C*------------------------------------------------------------------------*
049200970526     C* MODIFI - OPERAZIONI DI MODIFICA
049300970526     C*------------------------------------------------------------------------*
049400970526     C     MODIFI        BEGSR
049500970526     C*
049600970526     C* CONTROLLA I CAMPI IMMESSI
049700970526     C                   EXSR      DECODI
049800000719     C*--------------------
049900000719     C* codice strategi
050000000719     C*--------------------
050100000731     C                   IF        chk = '1' and giro = 1
050200000719     C                   MOVEL     '1'           ERRL00
050300000719     C                   ENDIF
050400970526     C*
050500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
050600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
050700991104     C                   EXSR      LETvss
050800970526     C*
050900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
051000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051100991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
051200110214     C*
051300110214     C                   SETOFF                                       41
051400110214     C                   IF        DDEVID > datcor or
051500110214     C                             DSCVID < datcor
051600110214     C                   SETON                                        41
051700110214     C                   ENDIF
051800110214     C*
051900110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
052000110214     C   41              EXSR      EXEPOST
052100110214     C*
052200991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
052300991104     C                   UPDATE    tivss000
052400101008     C*
052500101008     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
052600101008     C                   EXSR      EXESGI
052700101027     C                   EVAL      s20MSG = MSG(3)
052800101027     C                   EVAL      DSMSMS = MSG(3)
052900101022     C*
053000101022     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
053100101022     C                   IF        *IN80
053200111216     C                             and AS400_ok = *on
053300101022      /FREE
053400101022
053500101022         if UBBVSQL01_UpdUnqUtente(
053600101022                         pInConn
053700101022                       : %subst(VIDKSU:2:7)
053800101022                       : VIDPWDFTP) = 0;
053900101027         else;
054000101027              s20MSG = MSG(36);
054100101027              DSMSMS = MSG(36);
054200101022         endif;
054300101022
054400101022      /END-FREE
054500101022     C
054600101022     C                   ENDIF
054700101008     C*
054800000719     C                   Z-ADD     *zeros        giro
054900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
055000110214     C*
055100110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
055200110214     C  N41              EXSR      EXEPOST
055300110214     C*
055400970526E   2C                   ENDIF
055500970526E   1C                   ENDIF
055600970526     C*
055700970526     C                   ENDSR
055800991014     C*------------------------------------------------------------------------*
055900000719     C* COPIA  - OPERAZIONI DI COPIA
056000991014     C*------------------------------------------------------------------------*
056100000719     C     COPIA         BEGSR
056200991014     C*
056300991014     C* CONTROLLA I CAMPI IMMESSI
056400991014     C                   EXSR      DECODI
056500000719     C*--------------------
056600000719     C* codice strategi
056700000719     C*--------------------
056800000731     C                   IF        chk = '1' and giro = 1
056900000719     C                   MOVEL     '1'           ERRL00
057000000719     C                   ENDIF
057100991014     C*
057200991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
057300991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
057400991105     C                   MOVEL     VIDsun        vsssun                         *CHIAVE
057500991104     C                   MOVEL     vidisv        vssisv
057600991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
057700991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
057800001116     C                   WRITE     tivss000
057900000719     C                   Z-ADD     *zeros        giro
058000991104     C                   MOVEL     MSG(3)        s20MSG
058100000719     C                   MOVEL     MSG(3)        DSMSMS
058200991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
058300010612     C*
058400010612     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
058500010612     C                   IF        ERRL01 = '0'  and
058600010612     C                             VIDisv = 'TT'
058700010626 ORM C*                  EXSR      IMPvir
058800010626 ORM C*                  WRITE     tivir000
058900010612     C                   ENDIF
059000040422     C*
059100040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
059200040422     C                   EXSR      EXESGI
059300101012     C*
059400101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
059500101012     C                   EXSR      EXEPOST
059600991014E   2C                   ENDIF
059700991014     C*
059800991014     C                   ENDSR
059900970526     C*------------------------------------------------------------------------*
060000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
060100970526     C*------------------------------------------------------------------------*
060200991104     C*    ANNULL        BEGSR
060300970526     C*
060400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
060500991104     C*                  EXSR      LETvss
060600970527     C*
060700970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
060800991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
060900991104IF  2C*    vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
061000991104IF  3C*    vssDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
061100991104     C*                  MOVEL     'A'           vssvat                         *FLAG ANNULLAMENTO
061200991104     C*                  MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
061300991104     C*                  UPDATE    tivss000                                     *ANNULLA
061400991104X   3C*                  ELSE                                                   *MAI TRASMESSO
061500991104     C*                  DELETE    tivss000                                     *CANCELLA
061600991104E   3C*                  ENDIF
061700991104E   2C*                  ENDIF
061800991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
061900991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
062000991104E   1C*                  ENDIF
062100970526     C*
062200991104     C*                  ENDSR
062300021024     C*------------------------------------------------------------------------*
062400021024     C* ANNULL - OPERAZIONI DI FAR SCADERE
062500021024     C*------------------------------------------------------------------------*
062600021024     C     ANNULL        BEGSR
062700021024     C*
062800021024     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
062900021024     C                   EXSR      LETvss
063000021024     C*
063100021024     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
063200021024IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
063300021024IF  2C     vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
063400090611     C* Faccio scadere a oggi - 1 giorno
063500090611     C     *ISO          MOVE      datcor        wDataISO
063600090611     C     wDataISO      SUBDUR    1:*D          wDataISO
063700110214     C     *ISO          MOVE      wDataISO      DSCVID
063800090611     C     *ISO          MOVE      wDataISO      vssdsc
063900090611     C*
064000021024     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
064100110214     C*
064200110214     C                   SETOFF                                       41
064300110214     C                   IF        DDEVID > datcor or
064400110214     C                             DSCVID < datcor
064500110214     C                   SETON                                        41
064600110214     C                   ENDIF
064700110214     C*
064800110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
064900110214     C   41              EXSR      EXEPOST
065000110214     C*
065100021024     C                   UPDATE    tivss000                                     *ANNULLA
065200110214     C*
065300110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
065400110214     C  N41              EXSR      EXEPOST
065500110214     C*
065600021024E   2C                   ENDIF
065700021024     C                   MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
065800021024     C                   MOVEL     '1'           FINL00                         *FINE PGM
065900021024E   1C                   ENDIF
066000021024     C*
066100021024     C                   ENDSR
066200970526     C*------------------------------------------------------------------------*
066300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
066400970526     C*------------------------------------------------------------------------*
066500970526     C     RIPRIS        BEGSR
066600970526     C*
066700970526     C* CONTROLLA I CAMPI IMMESSI
066800970526     C                   EXSR      DECODI
066900000719     C*--------------------
067000000719     C* codice strategi
067100000719     C*--------------------
067200000731     C                   IF        chk = '1' and giro = 1
067300000719     C                   MOVEL     '1'           ERRL00
067400000719     C                   ENDIF
067500970526     C*
067600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
067700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
067800991104     C                   EXSR      LETvss
067900970526     C*
068000970526     C* IMPOSTA IL RECORD DA SCRIVERE
068100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
068200991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
068300991104     C                   UPDATE    tivss000                                     *ANNULLA
068400000719     C                   Z-ADD     *zeros        giro
068500970526E   2C                   ENDIF
068600991104     C                   MOVEL     MSG(5)        s20MSG                         *MESSAGGIO
068700000719     C                   MOVEL     MSG(5)        DSMSMS                         *MESSAGGIO
068800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
068900101012     C*
069000101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
069100101012     C                   EXSR      EXEPOST
069200970526E   1C                   ENDIF
069300970526     C*
069400970526     C                   ENDSR
069500970526     C*------------------------------------------------------------------------*
069600991104     C* LETvss - LEGGE IL RECORD PERL'AGGIORNAMENTO
069700970526     C*------------------------------------------------------------------------*
069800991104     C     LETvss        BEGSR
069900970526     C*
070000100322     C                   MOVE      vidsun        KCEsun
070100100924     C***                MOVE      vidisv        KCEisv
070200100924     C                   MOVE      vssisv        KCEisv
070300991104     C     KEYvss        CHAIN     tivss01L                           9998
070400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
070500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
070600991104     C                   MOVEL     MSG(7)        s20MSG                         *MESSAGGIO
070700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
070800970526E   1C                   ENDIF
070900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
071000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
071100991104     C                   MOVEL     MSG(6)        s20msg                         *MESSAGGIO
071200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
071300970526E   1C                   ENDIF
071400970526     C*
071500970526     C                   ENDSR
071600970526     C*------------------------------------------------------------------------*
071700970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
071800970526     C*------------------------------------------------------------------------*
071900970526     C     DECODI        BEGSR
072000000719     C*
072100000719     C                   MOVEL     '0'           ERRL00
072200000131     C*
072300991104     C*--------------------
072400991104     C* tipo servizio
072500991104     C*--------------------
072600991104IF  1C     ERRL00        IFEQ      '0'
072700991104IF  2C     VIDisv        IFne      *blank
072800020214     C                   EXSR      cntisv
072900991104X   2C                   ELSE
073000991104     C                   MOVEL     '1'           ERRL00
073100991104     C                   SETON                                        62
073200091117     C                   MOVEL     MSG(9)        DSMSMS
073300991104E   2C                   ENDIF
073400991104E   1C                   ENDIF
073500991008     C*--------------------
073600991008     C* tipo legame
073700991008     C*--------------------
073800991008IF  1C     ERRL00        IFEQ      '0'
073900020214     C*
074000100913     C* Verifico che il servizio del record corrente sia soggetto a controllo unificazioni
074100020214     C     VIDisv        lookup    isvkun                                 22
074200020214     C                   if        *in22 = *on
074300020214IF  2C     VIDtle        IFne      *blanks
074400091117     C     VIDtle        lookup    sktle                                  22
074500091117     C                   if        *in22 = *off
074600091117     C                   movel     '1'           errl00
074700091117     C                   seton                                        66
074800091117     C                   MOVEL     MSG(12)       DSMSMS
074900091117     C                   else
075000091117     C                   exsr      cnttle
075100091117     C                   endif
075200991008E   2C                   ENDIF
075300020214     C                   else
075400020214IF  2C     VIDtle        IFne      *blanks
075500020214     C                   MOVEL     '1'           ERRL00
075600020214     C                   SETON                                        66
075700020214     C                   MOVEL     MSG(28)       DSMSMS
075800020214E   2C                   ENDIF
075900020214     C                   endif
076000991008E   1C                   ENDIF
076100100913     C*
076200100913     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
076300100913     C     ERRL00        IFEQ      '0'
076400160705     C     VIDisv        lookup    isvftp                                 80
076500160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
076600160705     C                   setoff                                       80
076700160705     C                   endif
076800100913     C                   ENDIF
076900101022     C*
077000970528     C*--------------------
077100991008     C* codice padre
077200970528     C*--------------------
077300991105     C     *LIKE         DEFINE    vssksu        ksuVID
077400970528IF  1C     ERRL00        IFEQ      '0'
077500991105     C                   MOVEL     *BLANKS       VDDksu
077600991105IF  2C     VIDksu        IFEQ      *blanks                                      *STANDARD
077700020214IF  2C     VIDksu        OREQ      *zeros                                       *STANDARD
077800000705     C                   MOVEL     '1'           ERRL00
077900000705     C                   SETON                                        61
078000000705     C                   MOVEL     MSG(19)       DSMSMS
078100991105     C                   MOVEL     *ZEROS        ksuVID
078200970618X   2C                   ELSE
078300991105     C                   MOVEL     VIDksu        Wksu
078400991105     C                   MOVEL     *blank        Wdksu
078500991105     C                   EXSR      CHKksu
078600021114     C                   IF        flgKSU = 'S'
078700991105     C                   MOVEL     Wksu          VIDksu
078800991105     C                   MOVEL     WDksu         VDDksu
078900000728     C                   MOVEL     VIDksu        ksuVID
079000021114     C                   ENDIF
079100970618IF  3C     ERRL00        IFEQ      '1'
079200000728     C                   SETON                                        61
079300000728     C                   MOVEL     MSG(13)       DSMSMS
079400970618E   3C                   ENDIF
079500970618E   2C                   ENDIF
079600970528E   1C                   ENDIF
079700991015     C*--------------------
079800991015     C* DATA DECORRENZA
079900991015     C*--------------------
080000991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
080100991015IF  1C     ERRL00        IFEQ      '0'
080200991015     C                   Z-ADD     VIDDDE        wda8
080300991015     C                   Z-ADD     *ZEROS        da8w
080400991015     C                   EXSR      CHKDA8
080500991015     C                   Z-ADD     WDA8          viddde
080600991015     C                   Z-ADD     DA8W          ddevid
080700991015IF  2C     ERRL00        IFEQ      '1'
080800991015     C                   SETON                                        63
080900991015     C                   MOVEL     MSG(14)       dsmsms
081000991015E   2C                   ENDIF
081100991015E   1C                   ENDIF
081200991014     C* inserimento codice azienda (attualmente non abilitato)
081300991011IF  1C     ERRL00        IFEQ      '0'
081400991104IF  2C     VIDazp        IFne      *blank
081500991104     C                   SETON                                        60
081600991011     C                   MOVEL     MSG(19)       dsmsms
081700991011     C                   MOVEL     '1'           errl00
081800991011E   2C                   ENDIF
081900991011E   1C                   ENDIF
082000991011     C* INSERIMENTO -> RECORD INESISTENTE
082100991011IF  1C     ERRL00        IFEQ      '0'
082200991011IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
082300001116IF  2C     DEPOP1        OREQ      'O03'                                        *INSERIMENTO
082400991104     C                   MOVE      VIDisv        kceisv
082500001116     C                   MOVE      vidsun        kcesun
082600991104     C     KEYvss        CHAIN(N)  tivss01L                           99
082700970528IF  3C     *IN99         IFEQ      *OFF
082800970528     C                   MOVEL     '1'           ERRL00
082900970528     C                   MOVEL     MSG(8)        DSMSMS
083000970528     C                   SETON                                        606162
083100970528     C                   SETON                                        63
083200970528E   3C                   ENDIF
083300970528E   2C                   ENDIF
083400000317E   1C                   ENDIF
083500000317     C*---
083600000317     C* INSERIMENTO -> record inesistente come figlio di un cliente unificante per il servizio
083700010522     C*                record inesistente come padre (se stesso) con altro utente strategi
083800000317     C*---
083900000317     C* Cerca il padre se c'è
084000001116     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
084100001116     C     ERRL00        IFEQ      '0'
084200001116     C                   CLEAR                   tibs10ds
084300001116     C                   EVAL      d10drf = datcor                              *data riferimento
084400010522     C                   EVAL      d10tle = vidtle                              *legame INTERNET
084500001116     C                   EVAL      d10paf = 'P'                                 *se esiste record
084600001116     C                   MOVE      ksuvid        n8                8 0
084700001116     C                   Z-ADD     n8            d10cod                         *cliente figlio
084800001116     C                   CALL      'TIBS10R'                                    *cerca figli
084900001116     C                   PARM                    tibs10ds
085000001116     C                   IF        d10err = ' '                                 *esiste padre
085100001116     C                   MOVE      d10cop        kceksu
085200001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
085300000317     C     KE2vss        SETLL     tivss02L
085400000317     C     KE2vss        READE     tivss02L                               99
085500001116     C     *IN99         DOWEQ     *OFF
085600001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
085700001116     C     ddevid        ANDLE     vssdsc
085800001116     C                   MOVEL     MSG(10)       DSMSMS
085900001116     C                   LEAVE
086000001116     C                   ENDIF
086100001116     C     KE2vss        READE     tivss02L                               99
086200001116     C                   ENDDO
086300001116     C                   ELSE                                                   *NON esiste padre
086400000317     C                   MOVE      ksuvid        kceksu
086500001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
086600000317     C     KE2vss        SETLL     tivss02L
086700000317     C     KE2vss        READE     tivss02L                               99
086800001116     C     *IN99         DOWEQ     *OFF
086900001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
087000001116     C     ddevid        ANDLE     vssdsc
087100001116     C                   MOVEL     MSG(11)       DSMSMS
087200001116     C                   LEAVE
087300001116     C                   ENDIF
087400000317     C     KE2vss        READE     tivss02L                               99
087500001116e   4C                   ENDDO
087600001116e   3C                   ENDIF
087700001116e   2C                   ENDIF
087800001116e   1C                   ENDIF
087900970528     C*--------------------
088000970528     C* DATA SCADENZA
088100970528     C*--------------------
088200970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
088300970528IF  1C     ERRL00        IFEQ      '0'
088400970528     C                   Z-ADD     VIDDSC        WDA8
088500970528     C                   Z-ADD     *ZEROS        DA8W
088600970528     C                   EXSR      CHKDA8
088700970528     C                   Z-ADD     WDA8          VIDDSC
088800970528     C                   Z-ADD     DA8W          DSCVID
088900970528IF  2C     ERRL00        IFEQ      '1'
089000970528     C                   SETON                                        64
089100970528     C                   MOVEL     MSG(14)       DSMSMS
089200970528E   2C                   ENDIF
089300970528E   1C                   ENDIF
089400970528     C* SCADENZA > DECORRENZA
089500970528IF  1C     ERRL00        IFEQ      '0'
089600970528IF  2C     DSCVID        IFLT      DDEVID
089700970619     C                   SETON                                        64
089800970528     C                   MOVEL     MSG(15)       DSMSMS
089900970528     C                   MOVEL     '1'           ERRL00
090000970528E   2C                   ENDIF
090100970528E   1C                   ENDIF
090200970528     C* SCADENZA >= DATA CORRENTE
090300010220IF  1C*    ERRL00        IFEQ      '0'
090400010220IF  2C*    DEPOP1        IFEQ      'O02'
090500010220     C*    DEPOP1        OREQ      'O07'
090600010220     C*    DEPOP1        OREQ      'F06'
090700010220IF  3C*    DSCVID        IFLT      DATCOR
090800010220     C*                  SETON                                        64
090900010220     C*                  MOVEL     MSG(16)       DSMSMS
091000010220     C*                  MOVEL     '1'           ERRL00
091100010220E   3C*                  ENDIF
091200010220E   2C*                  ENDIF
091300010220E   1C*                  ENDIF
091400970528     C*---
091500970528     C* DECORRENZA + SCADENZA
091600970528     C*---
091700970528     C* ACCAVALLAMENTO DATI
091800970528IF  1C     ERRL00        IFEQ      '0'
091900080110     C* Controllo solo x servizi diversi da tipo T&T
092000050118     C     vidisv        lookup    isvtt                                  22
092100050118     C                   if        *in22 = *off
092200991104     C                   MOVEL     VIDisv        KCEisv
092300050118     C                   move      ksuVID        KCEksu
092400040910     C     Ke2vss        SETLL     tivss02L
092500040910     C     Ke2vss        READE(N)  tivss02L                               99
092600970528DO  2C     *IN99         DOWEQ     *OFF
092700080110IF  3C                   IF        (DEPOP1<>'F06' AND DDEVID<>vssDDE) OR
092800080110     C                              DEPOP1= 'F06'
092900080110     C                   IF        (DDEVID>=vssDDE AND DDEVID<=vssDSC) OR
093000080110     C                             (DSCVID>=vssDDE AND DSCVID<=vssDSC) OR
093100080110     C                             (vssDDE>=DDEVID AND vssDSC<=DSCVID)
093200970528     C                   SETON                                        606162
093300970619     C                   SETON                                        6364
093400970528     C                   MOVEL     MSG(17)       DSMSMS
093500970528     C                   MOVEL     '1'           ERRL00
093600970528     C                   LEAVE
093700970528E   5C                   ENDIF
093800970528E   3C                   ENDIF
093900040910     C     Ke2vss        READE(N)  tivss02L                               99
094000970528E   2C                   ENDDO
094100970528E   1C                   ENDIF
094200050118     C                   endif
094300020214     C*
094400020214     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
094500020214     C     vidisv        lookup    isvkun                                 22
094600020214     C                   if        *in22 = *on
094700000131     C*--------------------
094800000131     C* Clienti figli collegati
094900000131     C*--------------------
095000000215     C                   MOVEL     'N'           $esikun                        *non esistono kun
095100001116     C                   CLEAR                   vidmsg
095200020212     C     ERRL00        IFEQ      '0'
095300020212     C     VIDtle        IFNE      *blanks                                      *c'è legame
095400000204     C                   MOVEL     vidtle        kkutle
095500000131     C                   MOVEL     *blanks       kkuazp
095600000204     C                   MOVE      vidksu        kkucop
095700000215     C     KEYkun        SETLL     tikun01l
095800000215     C     KEYkun        READE     tikun01l                               99
095900020212     C                   DOW       NOT *in99
096000020212     C     kunvat        IFEQ      *blanks
096100020212     C                   IF        kundsc <  ddevid  or                         *non valido
096200010522     C                             kundde >  dscvid
096300020212     C                   ELSE
096400060802     C                   IF        kundsc >= datcor
096500000215     C                   MOVEL     'S'           $esikun                        *esistono kun
096600001116     C                   LEAVE
096700060802     C                   ENDIF
096800020212     C                   ENDIF
096900020212     C                   ENDIF
097000000215     C     KEYkun        READE     tikun01l                               99
097100020212     C                   ENDDO
097200020212     C     $esikun       IFEQ      'N'                                          *non esistono kun
097300060802     C                   IF        kundsc >= datcor
097400020212     C                   MOVEL     des(1)        vidmsg
097500001116     C                   SETON                                        76        *attenzione
097600060802     C                   ENDIF
097700020212     C                   ENDIF
097800000215     C*
097900020212     C                   ELSE                                                   *non c'è legame
098000000215     C*
098100000215     C                   MOVE      vidksu        kkucop
098200000215     C     KE3kun        SETLL     tikun03l
098300000215     C     KE3kun        READE     tikun03l                               99
098400020212     C                   DOW       NOT *in99
098500020212     C     kunvat        IFEQ      *blanks
098600020212     C                   IF        kundsc <  ddevid  or                         *non valido
098700010522     C                             kundde >  dscvid
098800020212     C                   ELSE
098900000215     C                   MOVEL     'S'           $esikun                        *esistono kun
099000001116     C                   LEAVE
099100020212     C                   ENDIF
099200020212     C                   ENDIF
099300000808     C     KEYkun        READE     tikun01l                               99
099400020212     C                   ENDDO
099500020212     C     $esikun       IFEQ      'S'                                          *esistono kun
099600060802     C                   IF        kundsc >= datcor
099700001116     C                   MOVEL     des(2)        vidmsg
099800020212     C                   SETON                                        76        *attenzione
099900060802     C                   ENDIF
100000020212     C                   ENDIF
100100020212     C                   ENDIF
100200020212     C                   ENDIF
100300020211     C*--------------------
100400091027     C* Che nn sia cliente figlio
100500020211     C*--------------------
100600091117     C*    ERRL00        ifeq      '0'
100700091117     C*                  move(p)   vidksu        kkucof
100800091117     c*
100900091117     c* ciclo x tutti i legami d tipo "internet"
101000091117     c*                  z-add     1             jtle
101100091117     c*                  dow       jtle <= %elem(sktle)
101200091117     c*                  if        sktle(jtle) <> *blanks
101300091117     c*
101400091117     C*                  movel     sktle(jtle)   kkutle
101500091117     C*    ke6kun        setll     tikun06l
101600091117     C*    ke6kun        reade     tikun06l
101700091117     C*                  dow       not %eof(tikun06l)
101800091117     C*    kunvat        ifeq      *blanks                                      *annullati/sospesi
101900091117     C*                  if        kundsc <  ddevid  or                         *non valido
102000091117     C*                            kundde >  dscvid
102100091117     C*                  else                                                   *valido
102200091117     C*                  seton                                        61
102300091117     C*                  movel     MSG(27)       DSMSMS
102400091117     C*                  movel     '1'           ERRL00
102500091117     C*                  leave
102600091117     C*                  endif
102700091117     C*                  endif
102800091117     C*    ke6kun        reade     tikun06l
102900091117     C*                  enddo
103000091027     c*
103100091117     c*                  else
103200091117     c*                  leave
103300091117     c*                  endif
103400091117     c*
103500091117     c*                  add       1             jtle
103600091117     c*                  enddo
103700091117     C*                  endif
103800091117     C*
103900091117     C                   endif
104000000913     C*------------------------
104100010522     C* Verifico che in INS/COPIA non esista già il cliente per il tipo servizio DL (x dec+scad)
104200000913     C*------------------------
104300020212     C     ERRL00        IFEQ      '0'
104400010522     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') AND
104500010522     C                              VIDisv = 'DL'
104600000913     C                   MOVE(p)   vidksu        kceksu
104700001116     C                   MOVE(p)   vidisv        kceisv                         *stesso servizio
104800000913     C     KE2vss        SETLL     tivss02L
104900000913     C     KE2vss        READE     tivss02L                               99
105000020212     C     *IN99         DOWEQ     *OFF
105100020212     C     ddevid        IFGE      vssdde                                       *in decorrenza
105200001116     C     ddevid        ANDLE     vssdsc
105300001116     C                   MOVEL     MSG(24)       DSMSMS
105400000913     C                   MOVEL     '1'           ERRL00
105500001116     C                   LEAVE
105600020212     C                   ENDIF
105700000913     C     KE2vss        READE     tivss02L                               99
105800020212     C                   ENDDO
105900020212     C                   ENDIF
106000020212     C                   ENDIF
106100000731     C*--------------------
106200000731     C* codice strategi
106300000731     C*--------------------
106400020802     C                   MOVEL     VIDsun        depTIP            1
106500020802     C*
106600020802     C* Eseguo controllo compatibilità Codice STRATEGI rispotto al servizio indicato
106700020802     C     ERRL00        IFEQ      '0'
106800020802     C                   IF        depTIP = '9' and
106900020802     C                             §isvproc <> 'F'
107000020802     C                   SETON                                        6265
107100020802     C                   MOVEL     MSG(29)       DSMSMS
107200020802     C                   MOVEL     '1'           ERRL00
107300020802     C                   ENDIF
107400020802     C                   IF        depTIP <> '9' and
107500020802     C                             §isvproc <> 'S'
107600020802     C                   SETON                                        6265
107700020802     C                   MOVEL     MSG(29)       DSMSMS
107800020802     C                   MOVEL     '1'           ERRL00
107900020802     C                   ENDIF
108000020802     C                   ENDIF
108100020802     C*
108200020802     C* Eseguo controllo esistenza Codice STRATEGI solo se "sun" nn inizia x '9'
108300020212     C     ERRL00        IFEQ      '0'
108400020802     C                   IF        depTIP <> '9'
108500000731     C                   EXSR      scanfile
108600020212     C                   ENDIF
108700020802     C                   ENDIF
108800010605     C*
108900010605     C* SE QUI TUTTO OK E TIPO SERVIZIO = 'TT' ESEGUO CONTROLLO ED EVENTUALE INSERIMNETO
109000010605     C* ANCHE DEL RECORD PER GLI ORM DA INTERNET NELL'ABILITAZIONE TIPI FILE X CLIENTE (TIVIR00F)
109100010608     C     ERRL00        IFEQ      '0'
109200010608     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') and
109300010608     C                             VIDisv = 'TT'
109400010626 ORM C*                  EXSR      DECVIR
109500010605     C                   ENDIF
109600010605     C                   ENDIF
109700090804     C*--------------------
109800100217     C* SUBK / tipo servizio
109900090804     C*--------------------
110000090804     C     ERRL00        IFEQ      '0'
110100090804     C                   IF        VIDSUBK <> *blanks AND VIDISV <> 'TT'
110200090804     C                   MOVEL     '1'           ERRL00
110300090804     C                   SETON                                        67
110400090804     C                   MOVEL     MSG(30)       DSMSMS
110500090804     C                   ENDIF
110600090804     C                   ENDIF
110700100217     C*--------------------
110800100217     C* controlli specifici x servizio
110900100217     C*--------------------
111000100217     C     ERRL00        IFEQ      '0'
111100100217     C                   z-add     1             jISV              3 0
111200100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
111300100217     C                   if        *in22 = *on
111400100217     C                   if        isvCTRL(jISV) <> *blanks
111500100217     C                   select
111600100913     C* - 0001
111700100322     C                   when      isvCTRL(jISV) = '0001'
111800120327     C*
111900120328     C* se cliente annullato o bloccato per motivi di mancato pagamento
112000120327     C                   IF        ACOFlg <> *blanks
112100130314     C                             or ACOAbl <> *blanks
112200120327     C                   SETON                                        61
112300120327     C                   EVAL      ErrL00 = '1'
112400120327     C                   MOVEL     MSG(37)       DSMSMS
112500120327     C                   ENDIF
112600120327     C*
112700120327     C     ERRL00        IFEQ      '0'
112800100217     C                   eval      kceksu = vidksu
112900100217     C                   eval      kceisv = 'TT'
113000100217     C                   eval      kcesun = vidsun
113100100217     C     KEYvss02_C    chain     tivss02L
113200100217     C                   if        %found(tivss02l) and
113300100217     C                             vssdde <= datcor and
113400100217     C                             vssdsc >  datcor
113500100322     C*
113600100322     C     FUNKEY        IFEQ      '06'
113700100406     C*
113800100406      /FREE
113900100406
114000100406              Ub§3EWupd_Upd (VIDKSU : VIDSUN : 'A' : *off );
114100100406
114200100406
114300100406      /END-FREE
114400100322     C*
114500100406     C                   ENDIF
114600100217     C                   else
114700100217     C                   MOVEL     '1'           ERRL00
114800100217     C                   SETON                                        62
114900100913     C                   MOVEL     MSG(31)       DSMSMS
115000100217     C                   endif
115100120327     C                   endif
115200101006     C* Infine rileggo il record "originale"
115300101006     C                   eval      kceksu = vidksu
115400101006     C                   eval      kceisv = vidisv
115500101006     C                   eval      kcesun = vidsun
115600101006     C     KEYvss02_C    chain     tivss02L
115700100913     C*
115800100217     C                   endsl
115900100217     C                   endif
116000100217     C                   endif
116100100217     C                   ENDIF
116200100913     C*--------------------
116300100913     C* Password FTP
116400100913     C*--------------------
116500100913     C     ERRL00        IFEQ      '0'
116600100913     C                   IF        *IN80 = *on
116700100913     C                   IF        VIDPWDFTP = *blanks
116800100913     C                   MOVEL     '1'           ERRL00
116900100913     C                   SETON                                        68
117000100913     C                   MOVEL     MSG(32)       DSMSMS
117100100913     C                   ELSE
117200100913     C*
117300100913     C* Password => verifica caratteri consentiti
117400100913     C                   Z-ADD     1             pos               2 0
117500100913     C                   FOR       pos = 1 to %len(%trim(VIDPWDFTP))
117600100913     C                   IF        %lookup(%subst(%trim(VIDPWDFTP):pos:1)
117700100913     C                             :valAry) = *zeros
117800100913     C                   MOVEL     '1'           ERRL00
117900100913     C                   SETON                                        68
118000100913     C                   MOVEL     MSG(33)       DSMSMS
118100100913     C                   ENDIF
118200100913     C                   ENDFOR
118300100913     C*
118400100913     C                   IF        ERRL00 <> '1'
118500100913     C                   IF        %len(%trim(VIDPWDFTP)) < 8
118600100913     C                   MOVEL     '1'           ERRL00
118700100913     C                   SETON                                        68
118800100913     C                   MOVEL     MSG(34)       DSMSMS
118900100913     C                   ENDIF
119000100913     C                   ENDIF
119100100913     C                   ENDIF
119200100913     C                   ENDIF
119300100913     C                   ENDIF
119400100914     C*--------------------
119500100914     C* Verifica Servizio / "F18"
119600100914     C*--------------------
119700100914     C     ERRL00        IFEQ      '0'
119800100914     C                   IF        VIDISV <> 'IE' and FUNKEY = '18'
119900100914     C                   MOVEL     '1'           ERRL00
120000100914     C                   SETON                                        68
120100100914     C                   MOVEL     MSG(35)       DSMSMS
120200100914     C                   ENDIF
120300100914     C                   ENDIF
120400060214     C*
120500100217     C* Forzatura necessaria
120600060214     C                   IF        KNMUS = 'EDPFG'
120700060214     C                   SETOFF                                       9091
120800060214     C                   ENDIF
120900000731     C*
121000970528     C                   ENDSR
121100010605     C*------------------------------------------------------------------------*
121200010608     C* DECVIR - CONTROLLA CAMPI DA INSERIRE NEL FILE "REGOLE X CLIENTE" (TIVIR00F)
121300010605     C*------------------------------------------------------------------------*
121400010605     C     DECVIR        BEGSR
121500010608     C*
121600010608     C                   MOVEL     '0'           ERRL01
121700010605     C*
121800010605     C*--------------------
121900010605     C* INSERIMENTO -> RECORD INESISTENTE
122000010605     C*--------------------
122100010608     C     ERRL01        IFEQ      '0'
122200010605     C                   MOVE(P)   VIDksu        KCEksc
122300010605     C                   MOVEL     'TO'          KCEtip
122400010605     C                   Z-ADD     ddeVID        KCEdti
122500010605     C     KEYvi2        CHAIN(N)  tivir03L                           99
122600010605     C     *IN99         IFEQ      *ON
122700010605     C* DECORRENZA + SCADENZA  (ACCAVALLAMENTO DATI)
122800010605     C     KEYvir        SETLL     tivir03L
122900010605     C     KEYvir        READE(N)  tivir03L                               99
123000010605     C     *IN99         DOWEQ     *OFF
123100010605     C     ddeVID        IFGE      virdti                                       *DECORRENZA
123200010605     C     ddeVID        ANDLE     virdtf
123300010605     C     dscVID        ORGE      virdti                                       *SCADENZA
123400010605     C     dscVID        ANDLE     virdtf
123500010605     C     virdti        ORGE      ddeVID                                       *DECORRENZA
123600010605     C     virdtf        ANDLE     dscVID
123700010605     C                   MOVEL     MSG(25)       DSMSMS
123800010608     C                   MOVEL     '1'           ERRL01
123900010605     C                   LEAVE
124000010605     C                   ENDIF
124100010605     C     KEYvir        READE(N)  tivir03L                               99
124200010605     C                   ENDDO
124300010605     C                   ELSE
124400010605     C                   MOVEL     MSG(26)       DSMSMS
124500010608     C                   MOVEL     '1'           ERRL01
124600010605     C                   ENDIF
124700010605     C                   ENDIF
124800010605     C*
124900010605     C                   ENDSR
125000010605     C*------------------------------------------------------------------------*
125100010605     C* SCANFILE - Routine di scansione del file degli UTENTI di STRATEGI
125200010605     C*------------------------------------------------------------------------*
125300010605     C     SCANFILE      BEGSR
125400000719     C*
125500000719     C                   open      sgius
125600000719     C*
125700000719     C                   eval      chk = '1'
125800000719     C                   add       1             giro
125900130611     C     vidsun        chain(e)  sgius
126000130611     C                   if        %found(sgius)
126100130611     C                   eval      chk = '0'
126200130611     C                   endif
126300000719     C*
126400000719     C                   if        chk = '1'
126500000719     C                   movel     MSG(21)       DSMSMS                         *MESSAGGIO
126600000719     C                   endif
126700000719     C*
126800000719     C                   close     sgius
126900000719     C*
127000010605     C                   ENDSR
127100991008     C*------------------------------------------------------------------------*
127200000807     C* Cntisv - CONTROLLA codice tipo servizio a cui il cliente è abilitato
127300991008     C*------------------------------------------------------------------------*
127400001116     C     cntisv        begsr
127500001116     C                   clear                   TIBS02DS
127600001116     C*
127700991104     C     '?'           SCAN      Vidisv                                 30
127800001116     C*  Ricerca
127900001116     C                   eval      t02tla = 'L'
128000001116    1C                   IF        *IN30 = *ON
128100001116     C                   movel     'R'           t02mod
128200001116     C                   else
128300001116     C                   movel     'C'           t02mod
128400001116     C                   movel(p)  vidisv        t02ke1
128500001116     C                   end
128600001116     C                   movel     'ISV'         t02cod
128700001116     C                   movel     knsif         t02sif
128800991008     C                   CALL      'TIBS02R'
128900001116     C                   parm                    KPJBA
129000001116     C                   parm                    TIBS02DS
129100001116     C
129200001116     C*
129300991008    2C                   IF        T02err = *blanks
129400991104     C                   movel     T02ke1        Vidisv
129500020802     C                   movel(p)  T02uni        DSISV
129600020802     C                   movel     §isvdesc      vddisv
129700001116     C                   else
129800001116     C                   movel     '1'           errl00
129900001116     C                   seton                                        62
130000991104     C                   MOVEL     MSG(9)        DSMSMS
130100991008     C                   ENDIF
130200001116     C
130300001116     C                   endsr
130400000807     C*------------------------------------------------------------------------*
130500991104     C* Cnttle - CONTROLLA codice tipo legame
130600991104     C*------------------------------------------------------------------------*
130700001116     C     cnttle        begsr
130800001116     C                   clear                   TIBS02DS
130900001116     C*
131000991104     C     '?'           SCAN      Vidtle                                 30
131100001116     C*  Ricerca
131200001116     C                   eval      t02tla = 'L'
131300991104    1C                   IF        *IN30 = *ON
131400001116     C                   movel     'R'           t02mod
131500001116     C                   else
131600001116     C                   movel     'C'           t02mod
131700001116     C                   movel(p)  vidtle        t02ke1
131800001116     C                   end
131900991104     C                   movel     'TLE'         t02cod
132000001116     C                   movel     knsif         t02sif
132100001116     C                   CALL      'TIBS02R'
132200001116     C                   parm                    KPJBA
132300001116     C                   parm                    TIBS02DS
132400001116     C
132500001116     C*
132600991104    2C                   IF        T02err = *blanks
132700000807     C                   exsr      chkaut
132800000807     C                   if        errl00 = *blanks
132900991104     C                   movel     T02ke1        Vidtle
133000091027     C                   movel     T02uni        dstle
133100991104     C                   movel     §tledesc      Vddtle
133200000807     C                   endif
133300000807     C                   else
133400000807     C                   movel     '1'           errl00
133500000807     C                   seton                                        66
133600000807     C                   MOVEL     MSG(12)       DSMSMS
133700991104     C                   ENDIF
133800000807     C
133900000807     C                   endsr
134000000807     C*------------------------------------------------------------------------*
134100000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
134200000807     C*------------------------------------------------------------------------*
134300000807     C     CHKAUT        BEGSR
134400000807     C*
134500020903     C* VERIFICO CHE L'UTENTE CHE EFFETTUA LA RICHIESTA SIA UN EDP*
134600020903     C                   IF        %subst(KNMUS:1:3) = 'EDP'
134700020903     C                   ELSE
134800020903     C                   MOVEL     '1'           ERRL00
134900020903     C                   MOVEL     MSG(23)       DSMSMS
135000020903     C                   ENDIF
135100000807     C*
135200000807     C                   ENDSR
135300970528     C*------------------------------------------------------------------------*
135400991105     C* CHKksu - CONTROLLA codice padre figlio
135500970528     C*------------------------------------------------------------------------*
135600991105     C     CHKksu        BEGSR
135700970528     C*
135800991105     C     *LIKE         DEFINE    VIDksu        Wksu
135900991105     C     *LIKE         DEFINE    VDDksu        WDksu
136000001116     C                   MOVE      *zeros        Wksc
136100021114     C                   MOVEL     'S'           flgKSU            1
136200970528     C*
136300970528     C* RICHIESTO ELENCO
136400970528     C                   SETOFF                                           28
136500991105     C     '?'           SCAN      Wksu                                   28
136600970528IF  1C     *IN28         IFEQ      *ON
136700991008     C* RICHIESTO ELENCO CODICI
136800021114     C                   MOVEL     'N'           flgKSU
136900001116     C* CLIENTI
137000991008     C                   CLEAR                   DSXC
137100020924     C                   Z-ADD     DUTKCI        DXCCAP
137200991008     C                   CALL      'XCLIR'
137300991008     C                   PARM                    DSXC
137400991008     C* SCOMPATTA
137500991008     C                   MOVEA     DXCKSC        KSA                        ALF
137600991008DO  2C     1             DO        30            J                 3 0
137700991008     C                   MOVE      KSA(J)        DSKSA                      F
137800991008     C                   Z-ADD     DSKSN         KSC(J)                     M
137900991008E   2C                   ENDDO
138000991011     C                   MOVEL     KSC(1)        WKSC              7
138100060807     C                   MOVEL     *all'0'       WKSU
138200060807     C                   MOVE      WKSC          WKSU
138300991011     C                   MOVEL     '1'           $TESVD            1        TA
138400060807E   1C                   ENDIF
138500991008     C*
138600991008     C* CODICE CLIENTE NUMERICO
138700021114IF  1C     WKSC          IFLT      *ZEROS
138800021114     C                   MOVEL     '1'           WERR              1
138900021114X   1C                   ELSE
139000021114     C*
139100970528     C* CONTROLLA ESISTENZA CODICE
139200021114     C                   MOVEL     'S'           flgKSU
139300991210     C                   MOVE      Wksu          wcli              7 0
139400001116     C                   EXSR      dectib
139500021114E   1C                   ENDIF
139600970528     C*
139700970528     C                   ENDSR
139800991210     C*------------------------------------------------------------------------*
139900001116     C* DECTIB - Controlla esistenza codice cliente
140000991210     C*------------------------------------------------------------------------*
140100001116     C     DECTIB        BEGSR
140200001116     C*
140300010913     C     VIDksu        CHAIN     tianv01l
140400010913     C                   IF        %found(tianv01l)
140500010913     C                   MOVEL(p)  anvdes        WDKsu
140600010913     C                   ELSE
140700050909     C*
140800050909     C* SOLO CARATTERI NUMERICI
140900050909     C   28              MOVE      *all'0'       wksu
141000050909     C   28              MOVE      wksc          wksu
141100050909     C                   SETOFF                                       28
141200050909     C                   TESTN                   Wksu                 28
141300050909IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
141400120327     C                   MOVEL     '1'           ERRL00
141500050909X   2C                   ELSE                                                   *E'NUMERO DA TESTN
141600050909     C                   MOVE      Wksu          W001              1
141700050909IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
141800050909     C                   MOVEL     '1'           ERRL00
141900050909X   3C                   ELSE                                                   *NUMERO
142000991210     C                   CLEAR                   BS69DS
142100991210     C                   CLEAR                   ACODS
142200991210     C                   MOVEL     KNSIF         I69SIF
142300991210     C                   Z-ADD     WCLI          I69KAC
142400991210     C                   CALL      'TIBS69R'
142500991210     C                   PARM                    BS69DS
142600991210     C                   PARM                    ACODS
142700991210     C                   PARM                    INDDS
142800991210     C                   PARM                    CLPDS
142900991210     C                   PARM                    CLSDS
143000991210     C     O69ERR        IFNE      '1'
143100991210     C                   MOVEL     ACORAG        wdksu
143200991210     C                   ELSE
143300000728     C                   MOVEL     *ALL'*'       Wdksu
143400001116     C                   MOVE      '1'           errl00
143500021114     C                   MOVEL     'N'           flgKSU
143600001116     C                   ENDIF
143700050909E   3C                   ENDIF
143800050909E   2C                   ENDIF
143900991210     C                   ENDIF
144000001116     C*
144100001116     C                   ENDSR
144200991210     C*------------------------------------------------------------------------*
144300991210     C* FINSR - OPERAZIONI FINALI
144400991210     C*------------------------------------------------------------------------*
144500991210     C     FINSR         BEGSR
144600991210     C*
144700991210     C* CHIUDE I FILE DEI PGM CHIAMATI
144800991210     C                   CLEAR                   bs69ds
144900991210     C                   CLEAR                   acods
145000991210     C                   CLEAR                   indds
145100991210     C                   CLEAR                   clpds
145200991210     C                   CLEAR                   clsds
145300991210     C                   MOVEL     'C'           i69tla
145400991210     C                   MOVEL     knsif         i69sif
145500991210     C                   CALL      'TIBS69R'
145600991210     C                   PARM                    bs69ds
145700991210     C                   PARM                    acods
145800991210     C                   PARM                    indds
145900991210     C                   PARM                    clpds
146000991210     C                   PARM                    clsds
146100991210     C*
146200991210     C                   ENDSR
146300970528     C*------------------------------------------------------------------------*
146400970528     C* CHKDA8 - CONTROLLA LA DATA (8)
146500970528     C*------------------------------------------------------------------------*
146600970528     C     CHKDA8        BEGSR
146700970528     C*
146800970528     C     *LIKE         DEFINE    VIDDDE        WDA8
146900970528     C     *LIKE         DEFINE    VIDDDE        DA8W
147000970528     C*
147100970528     C                   Z-ADD     WDA8          G08DAT
147200970528     C                   Z-ADD     *ZEROS        G08INV
147300970528     C                   MOVEL     '0'           G08ERR
147400970528     C                   CALL      'XSRDA8'
147500970528     C                   PARM                    WLBDA8
147600970528     C                   Z-ADD     G08DAT        WDA8
147700970528     C                   Z-ADD     G08INV        DA8W
147800970528IF  1C     G08ERR        IFEQ      '1'
147900970528     C                   MOVEL     '1'           ERRL00
148000970528E   1C                   ENDIF
148100970528     C*
148200970528     C                   ENDSR
148300970526     C*------------------------------------------------------------------------*
148400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
148500970526     C*------------------------------------------------------------------------*
148600970526     C     IREVID        BEGSR
148700970526     C* CAMPI
148800991104     C                   MOVEL     vsstle        VIDtle
148900991104     C                   MOVEL     vssisv        VIDisv
149000991104     C                   MOVEL     vssazp        VIDazp
149100991105     C                   MOVEL     vsssun        VIDsun
149200001116     C                   MOVE      vssksu        VIDksu
149300090804     C                   MOVEL     vssDTA        DVSSDTA
149400090804     C                   MOVEL     §VSSSUBK      VIDSUBK
149500991104     C                   Z-ADD     vssDDE        G08INV
149600970528     C                   Z-ADD     *ZEROS        G08DAT
149700970528     C                   MOVEL     '3'           G08ERR
149800970528     C                   CALL      'XSRDA8'
149900970528     C                   PARM                    WLBDA8
150000970528     C                   Z-ADD     G08DAT        VIDDDE
150100991104     C                   Z-ADD     vssDSC        G08INV
150200970528     C                   Z-ADD     *ZEROS        G08DAT
150300970528     C                   MOVEL     '3'           G08ERR
150400970528     C                   CALL      'XSRDA8'
150500970528     C                   PARM                    WLBDA8
150600051013     C                   Z-ADD     G08DAT        VIDDSC
150700051013     C*
150800051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
150900051013     C                   IF        DEPOP1 = 'F06' OR
151000051013     C                             DEPOP1 = 'O03' OR
151100051013     C                             DEPOP1 = 'O07'
151200051013     C                   EVAL      VIDDDE = DATCORINV
151300060726     C                   EVAL      VIDDSC = 31122039
151400051013     C                   ENDIF
151500970527     C*
151600970526     C                   ENDSR
151700970526     C*------------------------------------------------------------------------*
151800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
151900970526     C*------------------------------------------------------------------------*
152000970526     C     AZZVID        BEGSR
152100970526     C* CAMPI
152200000316     C*---               MOVEL     *BLANKS       VIDtle
152300991104     C                   MOVEL     *BLANKS       VIDisv
152400100913     C                   MOVEL     *BLANKS       VIDpwdftp
152500000316     C*---               MOVEL     *BLANKS       VIDazp
152600000316     C*---               Z-ADD     *ZEROS        VIDDDE
152700000316     C*---               Z-ADD     *ZEROS        VIDDSC
152800000316     C*---               move      *ZEROS        VIDksu
152900000316     C*---               move      *ZEROS        VIDsun
153000970526     C* DECODIFICHE
153100000316     C*---               MOVEL     *BLANKS       VDDtle
153200000215     C                   MOVEL     *BLANKS       VIDmsg
153300000316     C*---               MOVEL     *BLANKS       VDDksu
153400000316     C                   MOVEL     *BLANKS       VDDisv
153500970526     C*
153600970526     C                   ENDSR
153700970526     C*------------------------------------------------------------------------*
153800991104     C* IMPvss - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
153900970526     C*------------------------------------------------------------------------*
154000991104     C     IMPvss        BEGSR
154100970526     C*
154200991104     C                   MOVE      VIDazp        vssazp                         *CHIAVE
154300001116     C                   MOVE      ksuVID        vssksu
154400090804     C                   MOVEL     VIDSUBK       §VSSSUBK
154500090831     C                   EVAL      vssDTA = DVSSDTA
154600001116     C                   MOVE      VIDtle        vsstle
154700090714     C                   MOVE      VIDisv        vssISV
154800991104     C                   Z-ADD     DDEVID        vssDDE
154900991104     C                   Z-ADD     DSCVID        vssDSC
155000970526     C*
155100970526     C                   ENDSR
155200010605     C*------------------------------------------------------------------------*
155300010605     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE DELLE ABILITAZIONI CLIENTI/TIPI FILE (TIVIR)
155400010605     C*------------------------------------------------------------------------*
155500010605     C     IMPvir        BEGSR
155600010605     C*
155700010605     C                   MOVE      ksuVID        virksc
155800010605     C                   MOVE      'TO'          virtip
155900010605     C                   MOVE      'TT'          virfi1
156000010605     C                   MOVE      001           virseq
156100010605     C                   MOVE      999           virpoi
156200010605     C                   EVAL      virdes = 'ORM DA INTERNET'
156300010605     C                   MOVEL     'G '          virfi2
156400010605     C                   Z-ADD     DDEVID        virdti
156500010605     C                   Z-ADD     DSCVID        virdtf
156600010605     C*
156700010605     C                   ENDSR
156800970526     C*------------------------------------------------------------------------*
156900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
157000970526     C*------------------------------------------------------------------------*
157100970526     C     IMPFUN        BEGSR
157200970526     C*
157300970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
157400970526IF  1C     DEPOP1        IFEQ      'O05'
157500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
157600970526     C                   MOVEL     'N'           FUDATT
157700970526     C                   MOVEL     FUDRIG        FUD(6)
157800970526E   1C                   ENDIF
157900100914     C*
158000100914     C* SE NO INSERIMENTO e NO COPIOA => NO "F18"
158100100914     C                   IF        DEPOP1 <> 'F06' AND
158200100914     C                             DEPOP1 <> 'O03'
158300111216     C* NO F18 anche se l'AS400 non è SETRAS
158400111216     C                             or  AS400_ok = *off
158500100914     C                   MOVEL     FUD(18)       FUDRIG                         *AGGIORNAMENTO
158600100914     C                   MOVEL     'N'           FUDATT
158700100914     C                   MOVEL     FUDRIG        FUD(18)
158800100914     C                   ENDIF
158900970526     C*
159000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
159100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
159200970526     C                   EXSR      FUNGES
159300970526     C*
159400970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
159500970526     C*---
159600970526     C* 90 -> PROTEZIONE CHIAVE
159700970526     C* 91 -> PROTEZIONE TOTALE
159800030704     C* 92 -> PROTEZIONE PARZIALE
159900970526     C*---
160000970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
160100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
160200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
160300970526     C                   SETOFF                                       91
160400970526E   2C                   ENDIF
160500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
160600970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
160700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
160800970526E   2C                   ENDIF
160900970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
161000970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
161100970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
161200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
161300970526     C                   SETON                                        91
161400970526E   2C                   ENDIF
161500970526X   1C                   ELSE                                                   *I N T E R R O G A
161600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
161700970526     C                   SETON                                        91
161800970526E   1C                   ENDIF
161900030704     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
162000030704     C                   SETON                                        92        *SPROTEZIONE SCAD.
162100030704     C                   ENDIF
162200970526     C*
162300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
162400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
162500991104     C                   MOVEL     s20isv        KCEisv
162600001116     C                   MOVE      s20sun        KCEsun
162700991104     C     KEYvss        CHAIN     tivss01L                           9998
162800001116IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
162900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
163000970526     C                   MOVEL     MSG(6)        DSMSMS
163100970526X   2C                   ELSE
163200970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
163300980128IF  3C     DEPOP1        IFEQ      'O05'
163400001116     C     DEPOP1        OREQ      'O02'
163500101022     C*
163600101022     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
163700160705     C     vssISV        lookup    isvftp                                 80
163800160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
163900160705     C                   setoff                                       80
164000160705     C                   endif
164100101022     C                   IF        *IN80
164200111216     C                             and AS400_ok = *on
164300101022      /FREE
164400101022
164500101022         if UBBVSQL01_RtvUtente(
164600101022                         pInConn
164700101022                       : %subst(vssKSU:2:7)
164800101022                       : pOutNumRec
164900101022                       : pOutUsrPwd
165000101022                       : pOutUsrPath
165100101022                       : pOutUsrDesc
165200101022                       : pOutUsrFAnn
165300101022                       : pOutUsrEmail
165400101022                       : pOutUsrNote
165500101022                       : pOutUsrFList) = 0;
165600101022              if pOutNumRec = 1;
165700101022                 VIDPWDFTP = pOutUsrPwd;
165800101022              endif;
165900101022         endif;
166000101022
166100101022      /END-FREE
166200101022     C
166300101022     C                   ENDIF
166400101022     C*
166500101022     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
166600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
166700980128E   3C                   ENDIF
166800970526E   2C                   ENDIF
166900970526X   1C                   ELSE                                                   *INSERIMENTO
167000970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
167100970526E   1C                   ENDIF
167200970526     C*
167300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
167400970526IF  1C     DEPOP1        IFEQ      'O02'
167500970526     C                   MOVEL     DEC(1)        VIDFUN
167600970526E   1C                   ENDIF
167700991014IF  1C     DEPOP1        IFEQ      'O03'
167800991014     C                   MOVEL     DEC(6)        VIDFUN
167900991014E   1C                   ENDIF
168000970526IF  1C     DEPOP1        IFEQ      'O04'
168100970526     C                   MOVEL     DEC(2)        VIDFUN
168200970526E   1C                   ENDIF
168300970526IF  1C     DEPOP1        IFEQ      'O05'
168400970526     C                   MOVEL     DEC(3)        VIDFUN
168500970526E   1C                   ENDIF
168600970526IF  1C     DEPOP1        IFEQ      'O07'
168700970526     C                   MOVEL     DEC(4)        VIDFUN
168800970526E   1C                   ENDIF
168900970526IF  1C     DEPOP1        IFEQ      'F06'
169000970526     C                   MOVEL     DEC(5)        VIDFUN
169100970526E   1C                   ENDIF
169200970526     C*
169300970526     C                   ENDSR
169400020212     C*--------------------------------------------------------------------------------------------*
169500020212     C* CARTAB - carica le tabelle occorrenti
169600020212     C*--------------------------------------------------------------------------------------------*
169700020212     C     CARTAB        BEGSR
169800020212     C*
169900020212     C* Tipi servizi soggetti a controllo unificazioni
170000020212     C                   z-add     *zeros        x                 3 0
170100050118     C                   z-add     *zeros        y                 3 0
170200100217     C                   z-add     *zeros        k                 3 0
170300100913     C                   z-add     *zeros        ww                3 0
170400020212     C                   eval      tbecod = 'ISV'
170500091027     C     tbecod        setll     tntbe01l
170600091027     C     tbecod        reade     tntbe01l
170700020212     C                   dow       not %eof(tntbe01l)
170800020212     C                   movel     tbeuni        dsisv
170900020212     C                   if        §isvuni  = 'S'
171000020212     C                   add       1             x
171100020212     C                   eval      isvkun(x) = tbeke1
171200020212     C                   endif
171300080110     C                   if        §isvrgr  = 'T'
171400050118     C                   add       1             y
171500050118     C                   eval      isvtt(y) = tbeke1
171600050118     C                   endif
171700100913     C                   if        §isvftp  = 'F'
171800100913     C                   add       1             ww
171900100913     C                   eval      isvftp(ww) = tbeke1
172000100913     C                   endif
172100100217     C                   add       1             k
172200100217     C                   eval      isvKEYS(k)   = tbeke1
172300100217     C                   eval      isvCTRL(k)   = §ISVCTRL
172400101006     C                   eval      isvPGMAFT(k) = §ISVPGMSRV
172500091027     C     tbecod        reade     tntbe01l
172600020212     C                   enddo
172700091027     c*---
172800091027     c* tipi legami "internet"
172900091027     c*---
173000091027     c                   z-add     *zeros        jtle
173100091027     c                   movel     'TLE'         tbecod
173200091027     c     tbecod        setll     tntbe01l
173300091027     c     tbecod        reade     tntbe01l
173400091027     c                   dow       not %eof(tntbe01l)
173500091027     c                   if        tbeatb = *blanks
173600091027     c                   eval      dstle = tbeuni
173700091027     c* solo legami d tipo "internet"
173800091027     c                   if        §TLEGRP = 'W'
173900091027     c                   add       1             jtle
174000091027     c                   eval      sktle(jtle) = tbeke1
174100091027     c                   endif
174200091027     c                   endif
174300091027     c     tbecod        reade     tntbe01l
174400091027     c                   enddo
174500020212     C*
174600020212     C                   ENDSR
174700020924     C*--------------------------------------------------------------------------------------------*
174800020924     C* REPERISCE I DATI UTENTE
174900020924     C*--------------------------------------------------------------------------------------------*
175000020924     C     REPDATIUTE    BEGSR
175100020924     C*
175200020924     C* INIZIALIZZA VARIABILI DI WRK
175300020924     C                   CLEAR                   TIBS34DS
175400020924     C                   CLEAR                   AZUTEDS
175500020924     C                   CLEAR                   DDATIUTE
175600020924     C*
175700020924     C     *DTAARA       DEFINE    §azute        azuteds
175800020924     C     *DTAARA       DEFINE    §datiute      ddatiute
175900020924     C                   IN(E)     *DTAARA
176000020924if  1C                   IF        %Error
176100020924     c                   EVAL      I34Tla = 'L'
176200020924     C                   CALL      'TIBS34R'
176300020924     C                   PARM                    Tibs34Ds
176400020924     C                   IN        *DTAARA
176500020924e   1C                   ENDIF
176600020924     C*
176700020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
176800020924     C                   MOVEL(P)  RSUT          DSFIRS           20
176900020924     C*
177000020924     C                   ENDSR
177100040422     C*------------------------------------------------------------------------*
177200040422     C* EXESGI - GESTIONE PARAMETRI PARTICOLARI X UTENTE STRATEGI
177300040422     C*------------------------------------------------------------------------*
177400040422     C     EXESGI        BEGSR
177500040422     C*
177600040422     C* Inizializzo il flag che indica l'autorizzazione speciale da attribuire al servizio
177700040422     C                   movel     *blanks       wABILSGI          1
177800040422     C*
177900040422     C* Innanzitutto verifico se il servizio è d tipo STRATEGI e quale autorizzazione speciale
178000040422     C* prevede
178100040422     C                   eval      KTBcod = 'ISV'
178200040422     C                   eval      KTBky1 = VIDISV
178300040422     C     KEYtbe        chain     tntbe01l
178400040422     C                   if        %found(tntbe01l)
178500040422     C                   eval      DSISV = tbeUNI
178600040422     C                   if        §ISVPROC = 'S' AND
178700040422     C                             §ISVSGI1 <> *blanks
178800040422     C                   eval      wABILSGI = §ISVSGI1
178900040422     C                   endif
179000040422     C                   endif
179100040422     C*
179200040422     C* Se x il servizio è prevista una particolare abilitazione => procedo
179300040422     C                   if        wABILSGI <> *blanks
179400040422     C* Costrisco la stringa d comando base x impostare i parametri adeguati sull'utente STRATEGI
179500040422     C                   eval      CMD = 'STRATEGI/CHGSGIUSR ' +
179600040422     C                                   VIDSUN + '  ' +
179700040422     C                                   'ALWWEB(' + api + '*YES' + api + ') '
179800040422     C*
179900040422     C* Gestisco i vari parametri diversi da abilitare a seconda del servizio
180000040422     C                   if        wABILSGI = 'U'                               * UPLOAD
180100040422     C                   eval      CMD = %trim(CMD) + ' ' +
180200040422     C                             'ALWRCVFILE(' + api + '*YES' + api + ') '
180300040422     C                   endif
180400040422     C                   if        wABILSGI = 'D'                               * DOWNLOAD
180500040422     C                   eval      CMD = %trim(CMD) + ' ' +
180600040422     C                                'ALWFILE(' + api + '*YES' + api + ') '
180700040422     C                   endif
180800040422     C                   if        wABILSGI = 'S'                               * OUTQ
180900040422     C                   eval      CMD = %trim(CMD) + ' ' +
181000040422     C                              ' ALWSPOOL(' + api + '*YES' + api + ') '
181100040422     C                   endif
181200040422     C*
181300040422     C* Esegue comando remoto su AS777
181400040422     C                   call(e)   'TIS730C'
181500040422     C                   parm                    cmd
181600040422     C*
181700040422     C                   endif
181800040422     C*
181900040422     C                   ENDSR
182000090610     C*------------------------------------------------------------------------*
182100090610     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
182200090610     C*------------------------------------------------------------------------*
182300090610     C     CHKSGI        BEGSR
182400090714      /FREE
182500090714               if UBUTISGI_Init = 0;
182600090714               endif;
182700090714      /END-FREE
182800090611     C*
182900090610      /FREE
183000090610               pInSUN = vidSUN;
183100090610               pInKSU = vidKSU;
183200090611               pInRGR = 'S';
183300090610
183400090610               if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
183500090610               endif;
183600090610
183700090610               if pOutEsito = '0';
183800090610                  if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
183900090610                  endif;
184000110503               else;
184100090611                  if UBUTISGI_EnableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
184200090611                  endif;
184300090611               endif;
184400090611      /END-FREE
184500090611     C
184600090923     C
184700090714      /FREE
184800090714               if UBUTISGI_Finalize = 0;
184900090714               endif;
185000090714      /END-FREE
185100090714     C*
185200090610     C                   ENDSR
185300101022
185400101022
185500101022
185600101022     c*------------------------------------------------------------------------*
185700101022     c* opnCONN - Apertura connessione
185800101022     c*------------------------------------------------------------------------*
185900101022     c     OPNCONN       BEGSR
186000101022      /FREE
186100101022
186200101022        if not *in40;
186300101022           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
186400101022              pInConn = pOutConn;
186500101022           else;
186600101022              *in40 = *on;
186700101022              s725_§21_ERRFLG = 'E';
186800101022              s725_§21_ERRMSG = 'Errore BartVAS: fatal error ';
186900101022           endif;
187000101022        endif;
187100101022
187200101022      /END-FREE
187300101022      *
187400101022     c                   ENDSR
187500101022     c*------------------------------------------------------------------------*
187600101022
187700101022
187800101022
187900101022     c*------------------------------------------------------------------------*
188000101022     c* clsCONN - Chiusura connessione
188100101022     c*------------------------------------------------------------------------*
188200101022     c     CLSCONN       BEGSR
188300101022      /FREE
188400101022
188500101022           if UBBVSQL01_Disconnect(pInConn) = 0;
188600101022           else;
188700101022              *in40 = *on;
188800101022           endif;
188900101022
189000101022      /END-FREE
189100101022      *
189200101022     c                   ENDSR
189300101022     c*------------------------------------------------------------------------*
189400101022
189500101022
189600101022
189700970526     C*------------------------------------------------------------------------*
189800970526     C* *INZSR - OPERAZIONI INIZIALI
189900970526     C*------------------------------------------------------------------------*
190000970526     C     *INZSR        BEGSR
190100970526     C*---
190200970526     C* RICEVIMENTO PARAMETRI
190300970526     C*---
190400970526     C     *ENTRY        PLIST
190500970612     C                   PARM                    KPJBA
190600001116     C                   MOVEL     KPJBU         tis720
190700020924     C*
190800020924     C* REPERISCE I DATI DELL'UTENTE
190900020924     C                   EXSR      REPDATIUTE
191000970526     C*
191100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
191200970526     C     XMSGPA        PLIST
191300970526     C                   PARM                    DSMSMS
191400970526     C                   PARM                    DSMSPG
191500970526     C                   PARM                    DSMSKE
191600970526     C                   PARM                    DSMSER
191700970526     C*
191800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
191900970526     C                   CALL      'XKEY'
192000970526     C                   PARM                    DSKEY
192100020212     C*
192200020212     C* CARICA LE TABELLE OCCORRENTI
192300020212     C                   EXSR      CARTAB
192400111216     C*
192500111216     C* Reperisco il sistema AS/400 corrente
192600111216     C                   EVAL      AS400_ok = *off
192700111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
192800111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
192900111216     C                   EVAL      AS400_ok = *on
193000111216     C                   ENDIF
193100101022     C*
193200101022     C* Apro la connessione al DB "BARTVAS"
193300111216     C                   IF        AS400_ok = *on
193400101022     C                   exsr      opnCONN
193500111216     C                   ENDIF
193600001116     C*---
193700001116     C* VARIABILI NON RIFERITE AL DATA BASE
193800001116     C*---
193900001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
194000001116     C     *LIKE         DEFINE    s20OP0        DEPOP0
194100001116     C     *LIKE         DEFINE    s20OP1        DEPOP1
194200001116     C     *LIKE         DEFINE    s20OP0        WOP0
194300001116     C     *LIKE         DEFINE    s20OP1        WOP1
194400001116     C*---
194500001116     C* VARIABILI RIFERITE AL DATA BASE
194600001116     C*---
194700001116     C     *LIKE         DEFINE    vssisv        KCEisv
194800001116     C     *LIKE         DEFINE    vsssun        KCEsun
194900001116     C     *LIKE         DEFINE    vssksu        KCEksu
195000001116     C     *LIKE         DEFINE    kuntle        KKUtle
195100001116     C     *LIKE         DEFINE    kunazp        KKUazp
195200001116     C     *LIKE         DEFINE    kuncop        KKUcop
195300020211     C     *LIKE         DEFINE    kuncof        KKUcof
195400020211     C     *LIKE         DEFINE    kundde        KKUdde
195500001116     C     *LIKE         DEFINE    tbecod        KTBcod
195600001116     C     *LIKE         DEFINE    tbeke1        KTBky1
195700010605     C     *LIKE         DEFINE    virtip        KCEtip
195800010605     C     *LIKE         DEFINE    virksc        KCEksc
195900010605     C     *LIKE         DEFINE    virdti        KCEdti
196000001116     C*---
196100001116     C* VARIABILI DI CONTROLLO
196200001116     C*---
196300001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
196400001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
196500001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
196600001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
196700010608     C                   MOVEL     '0'           ERRL01            1            *ERRORE (x TIVIR)
196800001116     C*---
196900001116     C* CHIAVI DI LETTURA
197000001116     C*---
197100001116     C* LETTURA tivss01L - COMPLETA
197200001116     C     KEYvss        KLIST
197300001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
197400001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
197500100217     C* LETTURA tivss02L - PARZIALE
197600001116     C     KE2vss        KLIST
197700001116     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
197800001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
197900100217     C* LETTURA tivss02L - COMPLETA
198000100217     C     KEYvss02_C    KLIST
198100100217     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
198200100217     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
198300100217     C                   KFLD                    KCEsun                         *STRATEGI USER NBR
198400001116     C* LETTURA tikun01L
198500001116     C     KEYkun        KLIST
198600001116     C                   KFLD                    KKUtle                         *TIPO LEGAME
198700001116     C                   KFLD                    KKUazp                         *AZIENDA PADRE
198800001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
198900001116     C* LETTURA tikun03L
199000001116     C     KE3kun        KLIST
199100001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
199200020211     C* LETTURA tikun06L
199300020211     C     KE6kun        KLIST
199400020211     C                   KFLD                    KKUcof                         *CODICE CLI FIGLIO
199500020212     C                   KFLD                    KKUtle                         *TIPO LEGAME
199600001116     C* LETTURA tntbe01l
199700001116     C     KEYtbe        KLIST
199800001116     C                   KFLD                    KTBcod                         *CODICE TABELLA
199900001116     C                   KFLD                    KTBky1                         *CHIAVE 1 TABELLA
200000010605     C* LETTURA tivir03L - COMPLETA
200100010605     C     KEYvi2        KLIST
200200010605     C                   KFLD                    KCEtip                         *TIPO FILE
200300010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
200400010605     C                   KFLD                    KCEdti                         *DATA INIZIO ABIL.
200500010605     C* LETTURA tivir03L - INCOMPLETA
200600010605     C     KEYvir        KLIST
200700010605     C                   KFLD                    KCEtip                         *TIPO FILE
200800010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
200900970526     C*
201000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
201100991104     C                   MOVEL     'TIS725R'     DSMSPG
201200991104     C                   MOVEL     'TIS725R'     MSGQUE
201300970526     C                   MOVEL     *BLANKS       MSGKEY
201400970526     C                   MOVEL     *BLANKS       DSMSMS
201500970528     C*
201600970528     C* CALCOLA LA DATA CORRENTE
201700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
201800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
201900970528     C                   Z-ADD     WN8           G08DAT
202000051013     C                   Z-ADD     WN8           DATCORINV         8 0
202100970528     C                   Z-ADD     *ZEROS        G08INV
202200970528     C                   MOVEL     '0'           G08ERR
202300970528     C                   CALL      'XSRDA8'
202400970528     C                   PARM                    WLBDA8
202500970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
202600970526     C*
202700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
202800991104     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
202900991104     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
203000051013     C*
203100051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
203200051013     C                   IF        DEPOP1 = 'F06' OR
203300051013     C                             DEPOP1 = 'O03' OR
203400051013     C                             DEPOP1 = 'O07'
203500051013     C                   EVAL      VIDDDE = DATCORINV
203600060726     C                   EVAL      VIDDSC = 31122039
203700051013     C                   ENDIF
203800101022     C                   IF        DEPOP1 = 'F06' OR
203900101022     C                             DEPOP1 = 'O03'
204000101022     C                   SETOFF                                       81
204100101022     C                   ELSE
204200101022     C                   SETON                                        81
204300101022     C                   ENDIF
204400970526     C*
204500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
204600970526     C                   EXSR      IMPFUN
204700970526     C*
204800000131     C* ESEGUE SUBITO "DECODI" SE ENTRATI IN VISUALIZZAZIONE O MODIFICA
204900000915     C*    DEPOP1        IFEQ      'O05'
205000000915     C*    DEPOP1        OREQ      'O02'
205100000915     C*                  EXSR      DECODI
205200000915     C*                  ENDIF
205300000131     C*
205400970526     C                   ENDSR
205500970526     C*------------------------------------------------------------------------*
205600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
205700970526     C*------------------------------------------------------------------------*
205800970526     C     FUNGES        BEGSR
205900970526     C*
206000970526     C* PULISCO CAMPI DI LAVORO
206100970526     C                   Z-ADD     0             W                 5 0
206200970526     C*
206300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
206400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
206500970526     C     1             DO        24            I
206600970526     C                   MOVEL     FUD(I)        FUDRIG
206700970526     C*
206800970526     C* PULIZIA FUNZIONI PRECEDENTI
206900970526     C                   MOVEL     *BLANKS       FUV(I)
207000970526     C*
207100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
207200970526     C* (SALTA F24)
207300970526     C     FUDATT        IFEQ      'S'
207400970526     C     FUDFUN        ANDNE     '24'
207500970526     C                   ADD       1             W
207600970526     C                   MOVEL     FUDDES        FUV(W)
207700970526     C                   END
207800970526     C                   END                                                    DO WHILE
207900970526     C*
208000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
208100970526     C* ALTRIMENTI DISATTIVO F24
208200970526     C     1             DO        24            I
208300970526     C                   MOVEL     FUD(I)        FUDRIG
208400970526     C*
208500970526     C     FUDFUN        IFEQ      '24'
208600970526     C     W             IFGT      6
208700970526     C                   MOVEL     'S'           FUDATT
208800970526     C                   ELSE
208900970526     C                   MOVEL     *BLANKS       FUDATT
209000970526     C                   END                                                    W GT 8
209100970526     C                   MOVEL     FUDRIG        FUD(I)
209200970526     C                   END
209300970526     C                   END                                                    DO WHILE
209400970526     C*
209500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
209600970526     C     W             DIV       6             I
209700970526     C                   MVR                     Z
209800970526     C*
209900970526     C     Z             IFNE      0
210000970526     C     I             ADD       1             Z
210100970526     C                   ELSE
210200970526     C                   Z-ADD     I             Z                 5 0
210300970526     C                   END
210400970526     C*
210500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
210600970526     C                   ADD       1             FUNGRU
210700970526     C     FUNGRU        IFGT      Z
210800970526     C     FUNGRU        ORLT      1
210900970526     C                   Z-ADD     1             FUNGRU
211000970526     C                   END
211100970526     C*
211200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
211300970526     C     FUNGRU        IFEQ      1
211400970526     C                   MOVEL     FUV(1)        FUN001
211500970526     C                   MOVEL     FUV(2)        FUN002
211600970526     C                   MOVEL     FUV(3)        FUN003
211700970526     C                   MOVEL     FUV(4)        FUN004
211800970526     C                   MOVEL     FUV(5)        FUN005
211900970526     C                   MOVEL     FUV(6)        FUN006
212000970526     C                   END
212100970526     C*
212200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
212300970526     C     FUNGRU        IFEQ      2
212400970526     C                   MOVEL     FUV(7)        FUN001
212500970526     C                   MOVEL     FUV(8)        FUN002
212600970526     C                   MOVEL     FUV(9)        FUN003
212700970526     C                   MOVEL     FUV(10)       FUN004
212800970526     C                   MOVEL     FUV(11)       FUN005
212900970526     C                   MOVEL     FUV(12)       FUN006
213000970526     C                   END
213100970526     C*
213200970526     C* TERZO GRUPPO DI 6 FUNZIONI
213300970526     C     FUNGRU        IFEQ      3
213400970526     C                   MOVEL     FUV(13)       FUN001
213500970526     C                   MOVEL     FUV(14)       FUN002
213600970526     C                   MOVEL     FUV(15)       FUN003
213700970526     C                   MOVEL     FUV(16)       FUN004
213800970526     C                   MOVEL     FUV(17)       FUN005
213900970526     C                   MOVEL     FUV(18)       FUN006
214000970526     C                   END
214100970526     C*
214200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
214300970526     C     FUNGRU        IFEQ      4
214400970526     C                   MOVEL     FUV(19)       FUN001
214500970526     C                   MOVEL     FUV(20)       FUN002
214600970526     C                   MOVEL     FUV(21)       FUN003
214700970526     C                   MOVEL     FUV(22)       FUN004
214800970526     C                   MOVEL     *BLANKS       FUN005
214900970526     C                   MOVEL     *BLANKS       FUN006
215000970526     C                   END
215100970526     C*
215200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
215300970526     C                   MOVEL     *BLANKS       FUN007
215400970526     C                   MOVEL     *BLANKS       FUN008
215500970526     C*
215600970526     C     1             DO        24            I
215700970526     C                   MOVEL     FUD(I)        FUDRIG
215800970526     C     FUDFUN        IFEQ      '24'
215900970526     C     FUDATT        ANDEQ     'S'
216000970526     C                   MOVEL     FUDDES        FUN008
216100970526     C                   END
216200970526     C                   END                                                    DO
216300970526     C*
216400970526     C                   ENDSR
216500970526     C*------------------------------------------------------------------------*
216600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
216700970526     C*------------------------------------------------------------------------*
216800970526     C     FUNCON        BEGSR
216900970526     C*
217000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
217100970526     C                   EXSR      FUNVAR
217200970526     C*
217300970526     C* PULISCO CAMPI DI LAVORO
217400970526     C                   Z-ADD     0             I                 5 0
217500970526     C                   Z-ADD     0             W                 5 0
217600970526     C*
217700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
217800970526     C     FUNKEY        IFNE      *BLANKS
217900970526     C     1             DO        24            I
218000970526     C                   MOVEL     FUD(I)        FUDRIG
218100970526     C*
218200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
218300970526     C     FUNKEY        IFEQ      FUDFUN
218400970526     C     FUDATT        ANDEQ     'S'
218500970526     C                   Z-ADD     I             W
218600970526     C                   END
218700970526     C                   END                                                    DO WHILE
218800970526     C*
218900970526     C* FUNZIONE NON ATTIVA (ERRORE)
219000970526     C     W             IFEQ      0
219100970526     C*
219200970526     C* EMISSIONE MESSAGGIO DI ERRORE
219300970526     C                   MOVEL     MSG(1)        DSMSMS
219400970526     C*
219500970526     C                   END                                                    W=0
219600970526     C                   END                                                    FUNKEY NE *BLANKS
219700970526     C*
219800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
219900970526     C     W             IFNE      0
220000970526     C     FUNKEY        OREQ      *BLANKS
220100970526     C     FUNKEY        IFEQ      *BLANKS
220200970526     C                   EXSR      ENTGES
220300970526     C                   END
220400970526     C     FUNKEY        IFEQ      '03'
220500970526     C                   EXSR      F03GES
220600970526     C                   END
220700970526     C     FUNKEY        IFEQ      '06'
220800970526     C                   EXSR      F06GES
220900970526     C                   END
221000970526     C     FUNKEY        IFEQ      '12'
221100970526     C                   EXSR      F12GES
221200970526     C                   END
221300100914     C     FUNKEY        IFEQ      '18'
221400100914     C                   EXSR      F18GES
221500100914     C                   END
221600970526     C     FUNKEY        IFEQ      '24'
221700970526     C                   EXSR      F24GES
221800970526     C                   END
221900970526     C                   END
222000970526     C*
222100970526     C                   ENDSR
222200970526     C*------------------------------------------------------------------------*
222300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
222400970526     C*------------------------------------------------------------------------*
222500970526     C     FUNVAR        BEGSR
222600970526     C*
222700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
222800970526     C*
222900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
223000970526     C     KEY           IFEQ      ENTER
223100970526     C                   MOVEL     *BLANKS       FUNKEY
223200970526     C                   END
223300970526     C     KEY           IFEQ      F1
223400970526     C                   MOVEL     '01'          FUNKEY
223500970526     C                   END
223600970526     C     KEY           IFEQ      F2
223700970526     C                   MOVEL     '02'          FUNKEY
223800970526     C                   END
223900970526     C     KEY           IFEQ      F3
224000970526     C                   MOVEL     '03'          FUNKEY
224100970526     C                   END
224200970526     C     KEY           IFEQ      F4
224300970526     C                   MOVEL     '04'          FUNKEY
224400970526     C                   END
224500970526     C     KEY           IFEQ      F5
224600970526     C                   MOVEL     '05'          FUNKEY
224700970526     C                   END
224800970526     C     KEY           IFEQ      F6
224900970526     C                   MOVEL     '06'          FUNKEY
225000970526     C                   END
225100970526     C     KEY           IFEQ      F7
225200970526     C                   MOVEL     '07'          FUNKEY
225300970526     C                   END
225400970526     C     KEY           IFEQ      F8
225500970526     C                   MOVEL     '08'          FUNKEY
225600970526     C                   END
225700970526     C     KEY           IFEQ      F9
225800970526     C                   MOVEL     '09'          FUNKEY
225900970526     C                   END
226000970526     C     KEY           IFEQ      F10
226100970526     C                   MOVEL     '10'          FUNKEY
226200970526     C                   END
226300970526     C     KEY           IFEQ      F11
226400970526     C                   MOVEL     '11'          FUNKEY
226500970526     C                   END
226600970526     C     KEY           IFEQ      F12
226700970526     C                   MOVEL     '12'          FUNKEY
226800970526     C                   END
226900970526     C     KEY           IFEQ      F13
227000970526     C                   MOVEL     '13'          FUNKEY
227100970526     C                   END
227200970526     C     KEY           IFEQ      F14
227300970526     C                   MOVEL     '14'          FUNKEY
227400970526     C                   END
227500970526     C     KEY           IFEQ      F15
227600970526     C                   MOVEL     '15'          FUNKEY
227700970526     C                   END
227800970526     C     KEY           IFEQ      F16
227900970526     C                   MOVEL     '16'          FUNKEY
228000970526     C                   END
228100970526     C     KEY           IFEQ      F17
228200970526     C                   MOVEL     '17'          FUNKEY
228300970526     C                   END
228400970526     C     KEY           IFEQ      F18
228500970526     C                   MOVEL     '18'          FUNKEY
228600970526     C                   END
228700970526     C     KEY           IFEQ      F19
228800970526     C                   MOVEL     '19'          FUNKEY
228900970526     C                   END
229000970526     C     KEY           IFEQ      F20
229100970526     C                   MOVEL     '20'          FUNKEY
229200970526     C                   END
229300970526     C     KEY           IFEQ      F21
229400970526     C                   MOVEL     '21'          FUNKEY
229500970526     C                   END
229600970526     C     KEY           IFEQ      F22
229700970526     C                   MOVEL     '22'          FUNKEY
229800970526     C                   END
229900970526     C     KEY           IFEQ      F23
230000970526     C                   MOVEL     '23'          FUNKEY
230100970526     C                   END
230200970526     C     KEY           IFEQ      F24
230300970526     C                   MOVEL     '24'          FUNKEY
230400970526     C                   END
230500970526     C     KEY           IFEQ      ROLLUP
230600970526     C                   MOVEL     'RU'          FUNKEY
230700970526     C                   END
230800970526     C     KEY           IFEQ      HELP
230900970526     C                   MOVEL     'HP'          FUNKEY
231000970526     C                   END
231100970526     C                   ENDSR
231200970526     C*------------------------------------------------------------------------*
231300970526     C* NOTE COSTRUZIONE SCHIERE
231400970526     C*------------------------------------------------------------------------*
231500970526     C*
231600970526     C* -MSG-MESSAGGI
231700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
231800970526     C* (NON TOCCARE MSG,1 E MSG,2)
231900970526     C*
232000970526     C* -CMD-COMANDI
232100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
232200970526     C*
232300970526     C* -OPD-OPZIONI
232400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
232500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
232600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
232700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
232800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
232900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
233000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
233100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
233200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
233300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
233400970526     C*
233500970526     C* -FUD-FUNZIONI
233600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
233700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
233800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
233900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
234000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
234100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
234200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
234300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
234400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
234500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
234600970526     O*------------------------------------------------------------------------*
234700970526** DEC - DECODIFICHE
234800970526    MODIFICA                                                      1
234900970526  ANNULLAMENTO                                                    2
235000970526VISUALIZZAZIONE                                                   3
235100970526   RIPRISTINO                                                     4
235200970526  INSERIMENTO                                                     5
235300991105     COPIA                                                        6
235400000215** DES - DESCRIZIONI
235500000215ATTENZIONE: non esistono figli legati                             1
235600000215ATTENZIONE: esistono figli legati                                 2
235700020211                                                                  3
235800000215                                                                  4
235900000215                                                                  5
236000970526** MSG - MESSAGGI
236100991104TIS725R- Tasto funzionale non gestito                                          1
236200991104TIS725R- Inserimento effettuato                                                2
236300991104TIS725R- Modifica effettuata                                                   3
236400991104TIS725R- Annullamento effettuato                                               4
236500991104TIS725R- Ripristino effettuato                                                 5
236600991104TIS725R- Richiesta funzione per record inesistente                             6
236700991104TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
236800991104TIS725R- Richiesta funzione per record esistente                               8
236900991104TIS725R- Codice tipo servizio non presente in tabella                          9
237000000322TIS725R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
237100000317TIS725R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
237200091117TIS725R- Tipo legame inesistente o non utilizzabile                            12
237300991104TIS725R- Codice padre errato                                                   13
237400991104TIS725R- Data errata                                                           14
237500991104TIS725R- La data scadenza deve essere maggiore della decorrenza                15
237600991104TIS725R- Il record non può scadere in un giorno già trascorso                  16
237700991104TIS725R- Il record immesso si sovrappone ad uno già in decorrenza              17
237800991104TIS725R- Codice figlio errato                                                  18
237900000705TIS725R- Codice padre obbligatorio                                             19
238000991104TIS725R- Il codice figlio non può essere uguale al codice padre                20
238100000719TIS725R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
238200000807TIS725R- Tipo legame non autorizzato.                                          22
238300000807TIS725R- Filiale non abilitata alla gestione dei clienti scelti.               23
238400000913TIS725R- Servizio DL già presente nel periodo per il cliente scelto.           24
238500010605TIS725R- Nel periodo esistono già abilitazioni per il cliente al file TO.      25
238600010605TIS725R- Abilitazione per il cliente al file TO già esistente.                 26
238700020211TIS725R- Cliente già presente nei legami come figlio.                          27
238800020214TIS725R- Per il servizio indicato non è consentito specificare un legame.      28
238900020802TIS725R- Il codice Strategi è incompatibile con il servizio richiesto.         29
239000090804TIS725R- Campo valido solamente per servizi 'TT'                               30
239100100217TIS725R- E' necessaria una identica abilitazione al servizio 'TT'              31
239200100913TIS725R- Password necessaria x attivazione servizio richiesto.                 32
239300100913TIS725R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    33
239400100913TIS725R- Lunghezza minima password: 8 caratteri                                34
239500100914TIS725R- Tasto funzione non consentito per servizi diversi da 'IE'             35
239600101027TIS725R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       36
239700120327TIS725R- Cliente annullato o bloccato, impossibile abilitare EasyWeb           37
239800101027TIS725R-                                                                       38
239900101027TIS725R-                                                                       39
240000101027TIS725R-                                                                       40
240100970526** FUD - FUNZIONI
240200970526                   -        (S/N)=N                        funz.01¬
240300970526                   -        (S/N)=N                        funz.02¬
240400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
240500970526                   -        (S/N)=N                        funz.04¬
240600970526                   -        (S/N)=N                        funz.05¬
240700970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
240800970527                   -        (S/N)=N                        funz.07¬
240900970527                   -        (S/N)=N                        funz.08¬
241000970527                   -        (S/N)=N                        funz.09¬
241100970527                   -        (S/N)=N                        funz.10¬
241200970526                   -        (S/N)=N                        funz.11¬
241300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
241400970526                   -        (S/N)=N                        funz.13¬
241500970526                   -        (S/N)=N                        funz.14¬
241600970526                   -        (S/N)=N                        funz.15¬
241700970526                   -        (S/N)=N                        funz.16¬
241800970526                   -        (S/N)=N                        funz.17¬
241900100914F18=Conf.EasySped  -        (S/N)=S                        funz.18¬
242000970526                   -        (S/N)=N                        funz.19¬
242100970526                   -        (S/N)=N                        funz.20¬
242200970526                   -        (S/N)=N                        funz.21¬
242300970526                   -        (S/N)=N                        funz.22¬
242400970526                   -        (S/N)=N                        funz.23¬
242500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
