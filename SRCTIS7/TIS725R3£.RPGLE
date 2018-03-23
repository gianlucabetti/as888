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
038700170112     C* Gestisco Opzione di COPIA come un INSERIMENTO
038800170112     C                   if        S32OP1 = 'O03'
038900170112     C                   eval      S32OP1 = 'F06'
039000170112     C                   endif
039100170111     C                   eval      S32SUN = VIDSUN
039200170111     C                   eval      S32ISV = VIDISV
039300170111     C                   eval      S32TIP = 'VAS'
039400170111     C                   eval      S32KSU = VIDKSU
039500170111     C                   eval      ds_param = TIS732DS
039600101004     C*
039700101004     C                   endsl
039800100914     C*
039900101005     C   33              call      'TIS725R1'
040000100914     C                   parm                    KPJBA
040100100217     C                   parm                    wPGMAFTER
040200100217     C                   parm                    tivssds
040300100914     C                   parm                    ds_param
040400100914     C*
040500101006     C                   if        wPGMAFTER = '002' OR
040600101006     C                             wPGMAFTER = '003'
040700100914     C                   eval      TIS725DS21 = ds_param
040800100914     C                   if        s725_§21_ERRFLG <> *blanks
040900100914     C                   eval      s20MSG = s725_§21_ERRMSG
041000100914     C                   eval      DSMSMS = s725_§21_ERRMSG
041100100914     C                   endif
041200100914     C                   if        s725_§21_ERRFLG <> *blanks
041300100914     C                   MOVEL     '1'           ERRL00
041400100914     C                   endif
041500100914     C                   endif
041600100914     C*
041700100914     C                   eval      KPJBU = savKPJBU
041800100217     C                   endif
041900100217     C                   endif
042000101005     C*
042100101005     C                   endif
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* F12GES - GESTIONE F12 -> RITORNO
042600970526     C*------------------------------------------------------------------------*
042700970526     C     F12GES        BEGSR
042800970526     C*
042900970526     C* IMPOSTA "PREMUTO F12"
043000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
043100991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
043200970526     C*
043300970526     C                   ENDSR
043400100914     C*------------------------------------------------------------------------*
043500100914     C* F18GES - GESTIONE F18 -> AGGIORNAMENTO EASYSPED
043600100914     C*------------------------------------------------------------------------*
043700100914     C     F18GES        BEGSR
043800100914     C*
043900100914     C                   EXSR      F06GES
044000110419     C*
044100110419     C* Forzo l'elaborazione lato server
044200110419     C                   callp(e)  UBBVSKT01_ExeImmedCliTipF(
044300110419     C                                           '0ADMIN':'#U')
044400100914     C*
044500100914     C                   ENDSR
044600970526     C*------------------------------------------------------------------------*
044700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
044800970526     C*------------------------------------------------------------------------*
044900970526     C     F24GES        BEGSR
045000970526     C*
045100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
045200970526     C*
045300970526     C                   ENDSR
045400970526     C*------------------------------------------------------------------------*
045500010605     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
045600970526     C*------------------------------------------------------------------------*
045700970526     C     INSERI        BEGSR
045800970526     C*
045900970526     C* CONTROLLA I CAMPI IMMESSI
046000970526     C                   EXSR      DECODI
046100000731     C                   IF        chk = '1' and giro = 1
046200000719     C                   MOVEL     '1'           ERRL00
046300000719     C                   ENDIF
046400970526     C*
046500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
046600010605     C     ERRL00        IFEQ      '0'                                          *ANCORA NO ERRORI
046700000719     C                   MOVE      VIDsun        vsssun
046800010605     C                   MOVE      VIDisv        vssisv
046900991104     C                   EXSR      IMPvss                                       *ALTRI CAMPI
047000991104     C                   Z-ADD     *ZEROS        vssDTR                         *DATA TRASMISSIONE
047100991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
047200991104     C                   WRITE     tivss000
047300000719     C                   Z-ADD     *zeros        giro
047400100914     C                   MOVEL     MSG(2)        s20MSG                         *MESSAGGIO
047500100914     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
047600010605     C*
047700010605     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
047800010608     C                   IF        ERRL01 = '0'  and
047900010608     C                             VIDisv = 'TT'
048000010626 ORM C*                  EXSR      IMPvir
048100010626 ORM C*                  WRITE     tivir000
048200010605     C                   ENDIF
048300040422     C*
048400040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
048500040422     C                   EXSR      EXESGI
048600101012     C*
048700101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
048800101012     C                   EXSR      EXEPOST
048900970602     C*
049000970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
049100970602     C                   EXSR      AZZVID
049200010605     C                   ENDIF
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600970526     C* MODIFI - OPERAZIONI DI MODIFICA
049700970526     C*------------------------------------------------------------------------*
049800970526     C     MODIFI        BEGSR
049900970526     C*
050000970526     C* CONTROLLA I CAMPI IMMESSI
050100970526     C                   EXSR      DECODI
050200000719     C*--------------------
050300000719     C* codice strategi
050400000719     C*--------------------
050500000731     C                   IF        chk = '1' and giro = 1
050600000719     C                   MOVEL     '1'           ERRL00
050700000719     C                   ENDIF
050800970526     C*
050900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051100991104     C                   EXSR      LETvss
051200970526     C*
051300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
051400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051500991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
051600110214     C*
051700110214     C                   SETOFF                                       41
051800110214     C                   IF        DDEVID > datcor or
051900110214     C                             DSCVID < datcor
052000110214     C                   SETON                                        41
052100110214     C                   ENDIF
052200110214     C*
052300110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
052400110214     C   41              EXSR      EXEPOST
052500110214     C*
052600991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
052700991104     C                   UPDATE    tivss000
052800101008     C*
052900101008     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
053000101008     C                   EXSR      EXESGI
053100101027     C                   EVAL      s20MSG = MSG(3)
053200101027     C                   EVAL      DSMSMS = MSG(3)
053300101022     C*
053400101022     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
053500101022     C                   IF        *IN80
053600111216     C                             and AS400_ok = *on
053700101022      /FREE
053800101022
053900101022         if UBBVSQL01_UpdUnqUtente(
054000101022                         pInConn
054100101022                       : %subst(VIDKSU:2:7)
054200101022                       : VIDPWDFTP) = 0;
054300101027         else;
054400101027              s20MSG = MSG(36);
054500101027              DSMSMS = MSG(36);
054600101022         endif;
054700101022
054800101022      /END-FREE
054900101022     C
055000101022     C                   ENDIF
055100101008     C*
055200000719     C                   Z-ADD     *zeros        giro
055300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
055400110214     C*
055500110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
055600110214     C  N41              EXSR      EXEPOST
055700110214     C*
055800970526E   2C                   ENDIF
055900970526E   1C                   ENDIF
056000970526     C*
056100970526     C                   ENDSR
056200991014     C*------------------------------------------------------------------------*
056300000719     C* COPIA  - OPERAZIONI DI COPIA
056400991014     C*------------------------------------------------------------------------*
056500000719     C     COPIA         BEGSR
056600991014     C*
056700991014     C* CONTROLLA I CAMPI IMMESSI
056800991014     C                   EXSR      DECODI
056900000719     C*--------------------
057000000719     C* codice strategi
057100000719     C*--------------------
057200000731     C                   IF        chk = '1' and giro = 1
057300000719     C                   MOVEL     '1'           ERRL00
057400000719     C                   ENDIF
057500991014     C*
057600991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
057700991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
057800991105     C                   MOVEL     VIDsun        vsssun                         *CHIAVE
057900991104     C                   MOVEL     vidisv        vssisv
058000991104     C                   EXSR      IMPvss                                       *IMPOSTA I CAMPI
058100991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
058200001116     C                   WRITE     tivss000
058300000719     C                   Z-ADD     *zeros        giro
058400991104     C                   MOVEL     MSG(3)        s20MSG
058500000719     C                   MOVEL     MSG(3)        DSMSMS
058600991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
058700010612     C*
058800010612     C* SEMPRE SE SERVIZIO E' 'TT' VALORIZZO IL BUFFER DEL RECORD DEL TIVIR
058900010612     C                   IF        ERRL01 = '0'  and
059000010612     C                             VIDisv = 'TT'
059100010626 ORM C*                  EXSR      IMPvir
059200010626 ORM C*                  WRITE     tivir000
059300010612     C                   ENDIF
059400040422     C*
059500040422     C* ESEGUE LA ROUTINE X IMPOSTAZIONE UTENTE STRATEGI
059600040422     C                   EXSR      EXESGI
059700101012     C*
059800101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
059900101012     C                   EXSR      EXEPOST
060000991014E   2C                   ENDIF
060100991014     C*
060200991014     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
060500970526     C*------------------------------------------------------------------------*
060600991104     C*    ANNULL        BEGSR
060700970526     C*
060800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
060900991104     C*                  EXSR      LETvss
061000970527     C*
061100970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
061200991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
061300991104IF  2C*    vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
061400991104IF  3C*    vssDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
061500991104     C*                  MOVEL     'A'           vssvat                         *FLAG ANNULLAMENTO
061600991104     C*                  MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
061700991104     C*                  UPDATE    tivss000                                     *ANNULLA
061800991104X   3C*                  ELSE                                                   *MAI TRASMESSO
061900991104     C*                  DELETE    tivss000                                     *CANCELLA
062000991104E   3C*                  ENDIF
062100991104E   2C*                  ENDIF
062200991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
062300991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
062400991104E   1C*                  ENDIF
062500970526     C*
062600991104     C*                  ENDSR
062700021024     C*------------------------------------------------------------------------*
062800021024     C* ANNULL - OPERAZIONI DI FAR SCADERE
062900021024     C*------------------------------------------------------------------------*
063000021024     C     ANNULL        BEGSR
063100021024     C*
063200021024     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
063300021024     C                   EXSR      LETvss
063400021024     C*
063500021024     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
063600021024IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
063700021024IF  2C     vssvat        IFEQ      *BLANKS                                      *NO ANNULLATI
063800090611     C* Faccio scadere a oggi - 1 giorno
063900090611     C     *ISO          MOVE      datcor        wDataISO
064000090611     C     wDataISO      SUBDUR    1:*D          wDataISO
064100110214     C     *ISO          MOVE      wDataISO      DSCVID
064200090611     C     *ISO          MOVE      wDataISO      vssdsc
064300090611     C*
064400021024     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
064500110214     C*
064600110214     C                   SETOFF                                       41
064700110214     C                   IF        DDEVID > datcor or
064800110214     C                             DSCVID < datcor
064900110214     C                   SETON                                        41
065000110214     C                   ENDIF
065100110214     C*
065200110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
065300110214     C   41              EXSR      EXEPOST
065400110214     C*
065500021024     C                   UPDATE    tivss000                                     *ANNULLA
065600110214     C*
065700110214     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
065800110214     C  N41              EXSR      EXEPOST
065900110214     C*
066000021024E   2C                   ENDIF
066100021024     C                   MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
066200021024     C                   MOVEL     '1'           FINL00                         *FINE PGM
066300021024E   1C                   ENDIF
066400021024     C*
066500021024     C                   ENDSR
066600970526     C*------------------------------------------------------------------------*
066700970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
066800970526     C*------------------------------------------------------------------------*
066900970526     C     RIPRIS        BEGSR
067000970526     C*
067100970526     C* CONTROLLA I CAMPI IMMESSI
067200970526     C                   EXSR      DECODI
067300000719     C*--------------------
067400000719     C* codice strategi
067500000719     C*--------------------
067600000731     C                   IF        chk = '1' and giro = 1
067700000719     C                   MOVEL     '1'           ERRL00
067800000719     C                   ENDIF
067900970526     C*
068000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
068100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
068200991104     C                   EXSR      LETvss
068300970526     C*
068400970526     C* IMPOSTA IL RECORD DA SCRIVERE
068500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
068600991104     C                   MOVEL     *BLANKS       vssFTR                         *FLAG TRASMISSIONE
068700991104     C                   UPDATE    tivss000                                     *ANNULLA
068800000719     C                   Z-ADD     *zeros        giro
068900970526E   2C                   ENDIF
069000991104     C                   MOVEL     MSG(5)        s20MSG                         *MESSAGGIO
069100000719     C                   MOVEL     MSG(5)        DSMSMS                         *MESSAGGIO
069200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
069300101012     C*
069400101012     C* ESEGUE LA ROUTINE X OPERAZIONI "POST-AGGIORNAMENTO"
069500101012     C                   EXSR      EXEPOST
069600970526E   1C                   ENDIF
069700970526     C*
069800970526     C                   ENDSR
069900970526     C*------------------------------------------------------------------------*
070000991104     C* LETvss - LEGGE IL RECORD PERL'AGGIORNAMENTO
070100970526     C*------------------------------------------------------------------------*
070200991104     C     LETvss        BEGSR
070300970526     C*
070400100322     C                   MOVE      vidsun        KCEsun
070500100924     C***                MOVE      vidisv        KCEisv
070600100924     C                   MOVE      vssisv        KCEisv
070700991104     C     KEYvss        CHAIN     tivss01L                           9998
070800970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
070900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
071000991104     C                   MOVEL     MSG(7)        s20MSG                         *MESSAGGIO
071100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
071200970526E   1C                   ENDIF
071300970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
071400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
071500991104     C                   MOVEL     MSG(6)        s20msg                         *MESSAGGIO
071600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
071700970526E   1C                   ENDIF
071800970526     C*
071900970526     C                   ENDSR
072000970526     C*------------------------------------------------------------------------*
072100970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
072200970526     C*------------------------------------------------------------------------*
072300970526     C     DECODI        BEGSR
072400000719     C*
072500000719     C                   MOVEL     '0'           ERRL00
072600000131     C*
072700991104     C*--------------------
072800991104     C* tipo servizio
072900991104     C*--------------------
073000991104IF  1C     ERRL00        IFEQ      '0'
073100991104IF  2C     VIDisv        IFne      *blank
073200020214     C                   EXSR      cntisv
073300991104X   2C                   ELSE
073400991104     C                   MOVEL     '1'           ERRL00
073500991104     C                   SETON                                        62
073600091117     C                   MOVEL     MSG(9)        DSMSMS
073700991104E   2C                   ENDIF
073800991104E   1C                   ENDIF
073900991008     C*--------------------
074000991008     C* tipo legame
074100991008     C*--------------------
074200991008IF  1C     ERRL00        IFEQ      '0'
074300020214     C*
074400100913     C* Verifico che il servizio del record corrente sia soggetto a controllo unificazioni
074500020214     C     VIDisv        lookup    isvkun                                 22
074600020214     C                   if        *in22 = *on
074700020214IF  2C     VIDtle        IFne      *blanks
074800091117     C     VIDtle        lookup    sktle                                  22
074900091117     C                   if        *in22 = *off
075000091117     C                   movel     '1'           errl00
075100091117     C                   seton                                        66
075200091117     C                   MOVEL     MSG(12)       DSMSMS
075300091117     C                   else
075400091117     C                   exsr      cnttle
075500091117     C                   endif
075600991008E   2C                   ENDIF
075700020214     C                   else
075800020214IF  2C     VIDtle        IFne      *blanks
075900020214     C                   MOVEL     '1'           ERRL00
076000020214     C                   SETON                                        66
076100020214     C                   MOVEL     MSG(28)       DSMSMS
076200020214E   2C                   ENDIF
076300020214     C                   endif
076400991008E   1C                   ENDIF
076500100913     C*
076600100913     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
076700100913     C     ERRL00        IFEQ      '0'
076800160705     C     VIDisv        lookup    isvftp                                 80
076900160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
077000160705     C                   setoff                                       80
077100160705     C                   endif
077200100913     C                   ENDIF
077300101022     C*
077400970528     C*--------------------
077500991008     C* codice padre
077600970528     C*--------------------
077700991105     C     *LIKE         DEFINE    vssksu        ksuVID
077800970528IF  1C     ERRL00        IFEQ      '0'
077900991105     C                   MOVEL     *BLANKS       VDDksu
078000991105IF  2C     VIDksu        IFEQ      *blanks                                      *STANDARD
078100020214IF  2C     VIDksu        OREQ      *zeros                                       *STANDARD
078200000705     C                   MOVEL     '1'           ERRL00
078300000705     C                   SETON                                        61
078400000705     C                   MOVEL     MSG(19)       DSMSMS
078500991105     C                   MOVEL     *ZEROS        ksuVID
078600970618X   2C                   ELSE
078700991105     C                   MOVEL     VIDksu        Wksu
078800991105     C                   MOVEL     *blank        Wdksu
078900991105     C                   EXSR      CHKksu
079000021114     C                   IF        flgKSU = 'S'
079100991105     C                   MOVEL     Wksu          VIDksu
079200991105     C                   MOVEL     WDksu         VDDksu
079300000728     C                   MOVEL     VIDksu        ksuVID
079400021114     C                   ENDIF
079500970618IF  3C     ERRL00        IFEQ      '1'
079600000728     C                   SETON                                        61
079700000728     C                   MOVEL     MSG(13)       DSMSMS
079800970618E   3C                   ENDIF
079900970618E   2C                   ENDIF
080000970528E   1C                   ENDIF
080100991015     C*--------------------
080200991015     C* DATA DECORRENZA
080300991015     C*--------------------
080400991015     C     *LIKE         DEFINE    VIDDDE        DDEVID
080500991015IF  1C     ERRL00        IFEQ      '0'
080600991015     C                   Z-ADD     VIDDDE        wda8
080700991015     C                   Z-ADD     *ZEROS        da8w
080800991015     C                   EXSR      CHKDA8
080900991015     C                   Z-ADD     WDA8          viddde
081000991015     C                   Z-ADD     DA8W          ddevid
081100991015IF  2C     ERRL00        IFEQ      '1'
081200991015     C                   SETON                                        63
081300991015     C                   MOVEL     MSG(14)       dsmsms
081400991015E   2C                   ENDIF
081500991015E   1C                   ENDIF
081600991014     C* inserimento codice azienda (attualmente non abilitato)
081700991011IF  1C     ERRL00        IFEQ      '0'
081800991104IF  2C     VIDazp        IFne      *blank
081900991104     C                   SETON                                        60
082000991011     C                   MOVEL     MSG(19)       dsmsms
082100991011     C                   MOVEL     '1'           errl00
082200991011E   2C                   ENDIF
082300991011E   1C                   ENDIF
082400991011     C* INSERIMENTO -> RECORD INESISTENTE
082500991011IF  1C     ERRL00        IFEQ      '0'
082600991011IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
082700001116IF  2C     DEPOP1        OREQ      'O03'                                        *INSERIMENTO
082800991104     C                   MOVE      VIDisv        kceisv
082900001116     C                   MOVE      vidsun        kcesun
083000991104     C     KEYvss        CHAIN(N)  tivss01L                           99
083100970528IF  3C     *IN99         IFEQ      *OFF
083200970528     C                   MOVEL     '1'           ERRL00
083300970528     C                   MOVEL     MSG(8)        DSMSMS
083400970528     C                   SETON                                        606162
083500970528     C                   SETON                                        63
083600970528E   3C                   ENDIF
083700970528E   2C                   ENDIF
083800000317E   1C                   ENDIF
083900000317     C*---
084000000317     C* INSERIMENTO -> record inesistente come figlio di un cliente unificante per il servizio
084100010522     C*                record inesistente come padre (se stesso) con altro utente strategi
084200000317     C*---
084300000317     C* Cerca il padre se c'è
084400001116     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
084500001116     C     ERRL00        IFEQ      '0'
084600001116     C                   CLEAR                   tibs10ds
084700001116     C                   EVAL      d10drf = datcor                              *data riferimento
084800010522     C                   EVAL      d10tle = vidtle                              *legame INTERNET
084900001116     C                   EVAL      d10paf = 'P'                                 *se esiste record
085000001116     C                   MOVE      ksuvid        n8                8 0
085100001116     C                   Z-ADD     n8            d10cod                         *cliente figlio
085200001116     C                   CALL      'TIBS10R'                                    *cerca figli
085300001116     C                   PARM                    tibs10ds
085400001116     C                   IF        d10err = ' '                                 *esiste padre
085500001116     C                   MOVE      d10cop        kceksu
085600001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
085700000317     C     KE2vss        SETLL     tivss02L
085800000317     C     KE2vss        READE     tivss02L                               99
085900001116     C     *IN99         DOWEQ     *OFF
086000001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
086100001116     C     ddevid        ANDLE     vssdsc
086200001116     C                   MOVEL     MSG(10)       DSMSMS
086300001116     C                   LEAVE
086400001116     C                   ENDIF
086500001116     C     KE2vss        READE     tivss02L                               99
086600001116     C                   ENDDO
086700001116     C                   ELSE                                                   *NON esiste padre
086800000317     C                   MOVE      ksuvid        kceksu
086900001116     C                   MOVE      vidisv        kceisv                         *stesso servizio
087000000317     C     KE2vss        SETLL     tivss02L
087100000317     C     KE2vss        READE     tivss02L                               99
087200001116     C     *IN99         DOWEQ     *OFF
087300001116     C     ddevid        IFGE      vssdde                                       *in decorrenza
087400001116     C     ddevid        ANDLE     vssdsc
087500001116     C                   MOVEL     MSG(11)       DSMSMS
087600001116     C                   LEAVE
087700001116     C                   ENDIF
087800000317     C     KE2vss        READE     tivss02L                               99
087900001116e   4C                   ENDDO
088000001116e   3C                   ENDIF
088100001116e   2C                   ENDIF
088200001116e   1C                   ENDIF
088300970528     C*--------------------
088400970528     C* DATA SCADENZA
088500970528     C*--------------------
088600970528     C     *LIKE         DEFINE    VIDDSC        DSCVID
088700970528IF  1C     ERRL00        IFEQ      '0'
088800970528     C                   Z-ADD     VIDDSC        WDA8
088900970528     C                   Z-ADD     *ZEROS        DA8W
089000970528     C                   EXSR      CHKDA8
089100970528     C                   Z-ADD     WDA8          VIDDSC
089200970528     C                   Z-ADD     DA8W          DSCVID
089300970528IF  2C     ERRL00        IFEQ      '1'
089400970528     C                   SETON                                        64
089500970528     C                   MOVEL     MSG(14)       DSMSMS
089600970528E   2C                   ENDIF
089700970528E   1C                   ENDIF
089800970528     C* SCADENZA > DECORRENZA
089900970528IF  1C     ERRL00        IFEQ      '0'
090000970528IF  2C     DSCVID        IFLT      DDEVID
090100970619     C                   SETON                                        64
090200970528     C                   MOVEL     MSG(15)       DSMSMS
090300970528     C                   MOVEL     '1'           ERRL00
090400970528E   2C                   ENDIF
090500970528E   1C                   ENDIF
090600970528     C* SCADENZA >= DATA CORRENTE
090700010220IF  1C*    ERRL00        IFEQ      '0'
090800010220IF  2C*    DEPOP1        IFEQ      'O02'
090900010220     C*    DEPOP1        OREQ      'O07'
091000010220     C*    DEPOP1        OREQ      'F06'
091100010220IF  3C*    DSCVID        IFLT      DATCOR
091200010220     C*                  SETON                                        64
091300010220     C*                  MOVEL     MSG(16)       DSMSMS
091400010220     C*                  MOVEL     '1'           ERRL00
091500010220E   3C*                  ENDIF
091600010220E   2C*                  ENDIF
091700010220E   1C*                  ENDIF
091800970528     C*---
091900970528     C* DECORRENZA + SCADENZA
092000970528     C*---
092100970528     C* ACCAVALLAMENTO DATI
092200970528IF  1C     ERRL00        IFEQ      '0'
092300080110     C* Controllo solo x servizi diversi da tipo T&T
092400050118     C     vidisv        lookup    isvtt                                  22
092500050118     C                   if        *in22 = *off
092600991104     C                   MOVEL     VIDisv        KCEisv
092700050118     C                   move      ksuVID        KCEksu
092800040910     C     Ke2vss        SETLL     tivss02L
092900040910     C     Ke2vss        READE(N)  tivss02L                               99
093000970528DO  2C     *IN99         DOWEQ     *OFF
093100080110IF  3C                   IF        (DEPOP1<>'F06' AND DDEVID<>vssDDE) OR
093200080110     C                              DEPOP1= 'F06'
093300080110     C                   IF        (DDEVID>=vssDDE AND DDEVID<=vssDSC) OR
093400080110     C                             (DSCVID>=vssDDE AND DSCVID<=vssDSC) OR
093500080110     C                             (vssDDE>=DDEVID AND vssDSC<=DSCVID)
093600970528     C                   SETON                                        606162
093700970619     C                   SETON                                        6364
093800970528     C                   MOVEL     MSG(17)       DSMSMS
093900970528     C                   MOVEL     '1'           ERRL00
094000970528     C                   LEAVE
094100970528E   5C                   ENDIF
094200970528E   3C                   ENDIF
094300040910     C     Ke2vss        READE(N)  tivss02L                               99
094400970528E   2C                   ENDDO
094500970528E   1C                   ENDIF
094600050118     C                   endif
094700020214     C*
094800020214     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
094900020214     C     vidisv        lookup    isvkun                                 22
095000020214     C                   if        *in22 = *on
095100000131     C*--------------------
095200000131     C* Clienti figli collegati
095300000131     C*--------------------
095400000215     C                   MOVEL     'N'           $esikun                        *non esistono kun
095500001116     C                   CLEAR                   vidmsg
095600020212     C     ERRL00        IFEQ      '0'
095700020212     C     VIDtle        IFNE      *blanks                                      *c'è legame
095800000204     C                   MOVEL     vidtle        kkutle
095900000131     C                   MOVEL     *blanks       kkuazp
096000000204     C                   MOVE      vidksu        kkucop
096100000215     C     KEYkun        SETLL     tikun01l
096200000215     C     KEYkun        READE     tikun01l                               99
096300020212     C                   DOW       NOT *in99
096400020212     C     kunvat        IFEQ      *blanks
096500020212     C                   IF        kundsc <  ddevid  or                         *non valido
096600010522     C                             kundde >  dscvid
096700020212     C                   ELSE
096800060802     C                   IF        kundsc >= datcor
096900000215     C                   MOVEL     'S'           $esikun                        *esistono kun
097000001116     C                   LEAVE
097100060802     C                   ENDIF
097200020212     C                   ENDIF
097300020212     C                   ENDIF
097400000215     C     KEYkun        READE     tikun01l                               99
097500020212     C                   ENDDO
097600020212     C     $esikun       IFEQ      'N'                                          *non esistono kun
097700060802     C                   IF        kundsc >= datcor
097800020212     C                   MOVEL     des(1)        vidmsg
097900001116     C                   SETON                                        76        *attenzione
098000060802     C                   ENDIF
098100020212     C                   ENDIF
098200000215     C*
098300020212     C                   ELSE                                                   *non c'è legame
098400000215     C*
098500000215     C                   MOVE      vidksu        kkucop
098600000215     C     KE3kun        SETLL     tikun03l
098700000215     C     KE3kun        READE     tikun03l                               99
098800020212     C                   DOW       NOT *in99
098900020212     C     kunvat        IFEQ      *blanks
099000020212     C                   IF        kundsc <  ddevid  or                         *non valido
099100010522     C                             kundde >  dscvid
099200020212     C                   ELSE
099300000215     C                   MOVEL     'S'           $esikun                        *esistono kun
099400001116     C                   LEAVE
099500020212     C                   ENDIF
099600020212     C                   ENDIF
099700000808     C     KEYkun        READE     tikun01l                               99
099800020212     C                   ENDDO
099900020212     C     $esikun       IFEQ      'S'                                          *esistono kun
100000060802     C                   IF        kundsc >= datcor
100100001116     C                   MOVEL     des(2)        vidmsg
100200020212     C                   SETON                                        76        *attenzione
100300060802     C                   ENDIF
100400020212     C                   ENDIF
100500020212     C                   ENDIF
100600020212     C                   ENDIF
100700020211     C*--------------------
100800091027     C* Che nn sia cliente figlio
100900020211     C*--------------------
101000091117     C*    ERRL00        ifeq      '0'
101100091117     C*                  move(p)   vidksu        kkucof
101200091117     c*
101300091117     c* ciclo x tutti i legami d tipo "internet"
101400091117     c*                  z-add     1             jtle
101500091117     c*                  dow       jtle <= %elem(sktle)
101600091117     c*                  if        sktle(jtle) <> *blanks
101700091117     c*
101800091117     C*                  movel     sktle(jtle)   kkutle
101900091117     C*    ke6kun        setll     tikun06l
102000091117     C*    ke6kun        reade     tikun06l
102100091117     C*                  dow       not %eof(tikun06l)
102200091117     C*    kunvat        ifeq      *blanks                                      *annullati/sospesi
102300091117     C*                  if        kundsc <  ddevid  or                         *non valido
102400091117     C*                            kundde >  dscvid
102500091117     C*                  else                                                   *valido
102600091117     C*                  seton                                        61
102700091117     C*                  movel     MSG(27)       DSMSMS
102800091117     C*                  movel     '1'           ERRL00
102900091117     C*                  leave
103000091117     C*                  endif
103100091117     C*                  endif
103200091117     C*    ke6kun        reade     tikun06l
103300091117     C*                  enddo
103400091027     c*
103500091117     c*                  else
103600091117     c*                  leave
103700091117     c*                  endif
103800091117     c*
103900091117     c*                  add       1             jtle
104000091117     c*                  enddo
104100091117     C*                  endif
104200091117     C*
104300091117     C                   endif
104400000913     C*------------------------
104500010522     C* Verifico che in INS/COPIA non esista già il cliente per il tipo servizio DL (x dec+scad)
104600000913     C*------------------------
104700020212     C     ERRL00        IFEQ      '0'
104800010522     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') AND
104900010522     C                              VIDisv = 'DL'
105000000913     C                   MOVE(p)   vidksu        kceksu
105100001116     C                   MOVE(p)   vidisv        kceisv                         *stesso servizio
105200000913     C     KE2vss        SETLL     tivss02L
105300000913     C     KE2vss        READE     tivss02L                               99
105400020212     C     *IN99         DOWEQ     *OFF
105500020212     C     ddevid        IFGE      vssdde                                       *in decorrenza
105600001116     C     ddevid        ANDLE     vssdsc
105700001116     C                   MOVEL     MSG(24)       DSMSMS
105800000913     C                   MOVEL     '1'           ERRL00
105900001116     C                   LEAVE
106000020212     C                   ENDIF
106100000913     C     KE2vss        READE     tivss02L                               99
106200020212     C                   ENDDO
106300020212     C                   ENDIF
106400020212     C                   ENDIF
106500000731     C*--------------------
106600000731     C* codice strategi
106700000731     C*--------------------
106800020802     C                   MOVEL     VIDsun        depTIP            1
106900020802     C*
107000020802     C* Eseguo controllo compatibilità Codice STRATEGI rispotto al servizio indicato
107100020802     C     ERRL00        IFEQ      '0'
107200020802     C                   IF        depTIP = '9' and
107300020802     C                             §isvproc <> 'F'
107400020802     C                   SETON                                        6265
107500020802     C                   MOVEL     MSG(29)       DSMSMS
107600020802     C                   MOVEL     '1'           ERRL00
107700020802     C                   ENDIF
107800020802     C                   IF        depTIP <> '9' and
107900020802     C                             §isvproc <> 'S'
108000020802     C                   SETON                                        6265
108100020802     C                   MOVEL     MSG(29)       DSMSMS
108200020802     C                   MOVEL     '1'           ERRL00
108300020802     C                   ENDIF
108400020802     C                   ENDIF
108500020802     C*
108600020802     C* Eseguo controllo esistenza Codice STRATEGI solo se "sun" nn inizia x '9'
108700020212     C     ERRL00        IFEQ      '0'
108800020802     C                   IF        depTIP <> '9'
108900000731     C                   EXSR      scanfile
109000020212     C                   ENDIF
109100020802     C                   ENDIF
109200010605     C*
109300010605     C* SE QUI TUTTO OK E TIPO SERVIZIO = 'TT' ESEGUO CONTROLLO ED EVENTUALE INSERIMNETO
109400010605     C* ANCHE DEL RECORD PER GLI ORM DA INTERNET NELL'ABILITAZIONE TIPI FILE X CLIENTE (TIVIR00F)
109500010608     C     ERRL00        IFEQ      '0'
109600010608     C                   IF        (DEPOP1 = 'F06' or DEPOP1 = 'O03') and
109700010608     C                             VIDisv = 'TT'
109800010626 ORM C*                  EXSR      DECVIR
109900010605     C                   ENDIF
110000010605     C                   ENDIF
110100090804     C*--------------------
110200100217     C* SUBK / tipo servizio
110300090804     C*--------------------
110400090804     C     ERRL00        IFEQ      '0'
110500090804     C                   IF        VIDSUBK <> *blanks AND VIDISV <> 'TT'
110600090804     C                   MOVEL     '1'           ERRL00
110700090804     C                   SETON                                        67
110800090804     C                   MOVEL     MSG(30)       DSMSMS
110900090804     C                   ENDIF
111000090804     C                   ENDIF
111100100217     C*--------------------
111200100217     C* controlli specifici x servizio
111300100217     C*--------------------
111400100217     C     ERRL00        IFEQ      '0'
111500100217     C                   z-add     1             jISV              3 0
111600100217     C     VIDisv        lookup    isvKEYS(jISV)                          22
111700100217     C                   if        *in22 = *on
111800100217     C                   if        isvCTRL(jISV) <> *blanks
111900100217     C                   select
112000100913     C* - 0001
112100100322     C                   when      isvCTRL(jISV) = '0001'
112200120327     C*
112300120328     C* se cliente annullato o bloccato per motivi di mancato pagamento
112400120327     C                   IF        ACOFlg <> *blanks
112500130314     C                             or ACOAbl <> *blanks
112600120327     C                   SETON                                        61
112700120327     C                   EVAL      ErrL00 = '1'
112800120327     C                   MOVEL     MSG(37)       DSMSMS
112900120327     C                   ENDIF
113000120327     C*
113100120327     C     ERRL00        IFEQ      '0'
113200100217     C                   eval      kceksu = vidksu
113300100217     C                   eval      kceisv = 'TT'
113400100217     C                   eval      kcesun = vidsun
113500100217     C     KEYvss02_C    chain     tivss02L
113600100217     C                   if        %found(tivss02l) and
113700100217     C                             vssdde <= datcor and
113800100217     C                             vssdsc >  datcor
113900100322     C*
114000100322     C     FUNKEY        IFEQ      '06'
114100100406     C*
114200100406      /FREE
114300100406
114400100406              Ub§3EWupd_Upd (VIDKSU : VIDSUN : 'A' : *off );
114500100406
114600100406
114700100406      /END-FREE
114800100322     C*
114900100406     C                   ENDIF
115000100217     C                   else
115100100217     C                   MOVEL     '1'           ERRL00
115200100217     C                   SETON                                        62
115300100913     C                   MOVEL     MSG(31)       DSMSMS
115400100217     C                   endif
115500120327     C                   endif
115600101006     C* Infine rileggo il record "originale"
115700101006     C                   eval      kceksu = vidksu
115800101006     C                   eval      kceisv = vidisv
115900101006     C                   eval      kcesun = vidsun
116000101006     C     KEYvss02_C    chain     tivss02L
116100100913     C*
116200100217     C                   endsl
116300100217     C                   endif
116400100217     C                   endif
116500100217     C                   ENDIF
116600100913     C*--------------------
116700100913     C* Password FTP
116800100913     C*--------------------
116900100913     C     ERRL00        IFEQ      '0'
117000100913     C                   IF        *IN80 = *on
117100100913     C                   IF        VIDPWDFTP = *blanks
117200100913     C                   MOVEL     '1'           ERRL00
117300100913     C                   SETON                                        68
117400100913     C                   MOVEL     MSG(32)       DSMSMS
117500100913     C                   ELSE
117600100913     C*
117700100913     C* Password => verifica caratteri consentiti
117800100913     C                   Z-ADD     1             pos               2 0
117900100913     C                   FOR       pos = 1 to %len(%trim(VIDPWDFTP))
118000100913     C                   IF        %lookup(%subst(%trim(VIDPWDFTP):pos:1)
118100100913     C                             :valAry) = *zeros
118200100913     C                   MOVEL     '1'           ERRL00
118300100913     C                   SETON                                        68
118400100913     C                   MOVEL     MSG(33)       DSMSMS
118500100913     C                   ENDIF
118600100913     C                   ENDFOR
118700100913     C*
118800100913     C                   IF        ERRL00 <> '1'
118900100913     C                   IF        %len(%trim(VIDPWDFTP)) < 8
119000100913     C                   MOVEL     '1'           ERRL00
119100100913     C                   SETON                                        68
119200100913     C                   MOVEL     MSG(34)       DSMSMS
119300100913     C                   ENDIF
119400100913     C                   ENDIF
119500100913     C                   ENDIF
119600100913     C                   ENDIF
119700100913     C                   ENDIF
119800100914     C*--------------------
119900100914     C* Verifica Servizio / "F18"
120000100914     C*--------------------
120100100914     C     ERRL00        IFEQ      '0'
120200100914     C                   IF        VIDISV <> 'IE' and FUNKEY = '18'
120300100914     C                   MOVEL     '1'           ERRL00
120400100914     C                   SETON                                        68
120500100914     C                   MOVEL     MSG(35)       DSMSMS
120600100914     C                   ENDIF
120700100914     C                   ENDIF
120800060214     C*
120900100217     C* Forzatura necessaria
121000060214     C                   IF        KNMUS = 'EDPFG'
121100060214     C                   SETOFF                                       9091
121200060214     C                   ENDIF
121300000731     C*
121400970528     C                   ENDSR
121500010605     C*------------------------------------------------------------------------*
121600010608     C* DECVIR - CONTROLLA CAMPI DA INSERIRE NEL FILE "REGOLE X CLIENTE" (TIVIR00F)
121700010605     C*------------------------------------------------------------------------*
121800010605     C     DECVIR        BEGSR
121900010608     C*
122000010608     C                   MOVEL     '0'           ERRL01
122100010605     C*
122200010605     C*--------------------
122300010605     C* INSERIMENTO -> RECORD INESISTENTE
122400010605     C*--------------------
122500010608     C     ERRL01        IFEQ      '0'
122600010605     C                   MOVE(P)   VIDksu        KCEksc
122700010605     C                   MOVEL     'TO'          KCEtip
122800010605     C                   Z-ADD     ddeVID        KCEdti
122900010605     C     KEYvi2        CHAIN(N)  tivir03L                           99
123000010605     C     *IN99         IFEQ      *ON
123100010605     C* DECORRENZA + SCADENZA  (ACCAVALLAMENTO DATI)
123200010605     C     KEYvir        SETLL     tivir03L
123300010605     C     KEYvir        READE(N)  tivir03L                               99
123400010605     C     *IN99         DOWEQ     *OFF
123500010605     C     ddeVID        IFGE      virdti                                       *DECORRENZA
123600010605     C     ddeVID        ANDLE     virdtf
123700010605     C     dscVID        ORGE      virdti                                       *SCADENZA
123800010605     C     dscVID        ANDLE     virdtf
123900010605     C     virdti        ORGE      ddeVID                                       *DECORRENZA
124000010605     C     virdtf        ANDLE     dscVID
124100010605     C                   MOVEL     MSG(25)       DSMSMS
124200010608     C                   MOVEL     '1'           ERRL01
124300010605     C                   LEAVE
124400010605     C                   ENDIF
124500010605     C     KEYvir        READE(N)  tivir03L                               99
124600010605     C                   ENDDO
124700010605     C                   ELSE
124800010605     C                   MOVEL     MSG(26)       DSMSMS
124900010608     C                   MOVEL     '1'           ERRL01
125000010605     C                   ENDIF
125100010605     C                   ENDIF
125200010605     C*
125300010605     C                   ENDSR
125400010605     C*------------------------------------------------------------------------*
125500010605     C* SCANFILE - Routine di scansione del file degli UTENTI di STRATEGI
125600010605     C*------------------------------------------------------------------------*
125700010605     C     SCANFILE      BEGSR
125800000719     C*
125900000719     C                   open      sgius
126000000719     C*
126100000719     C                   eval      chk = '1'
126200000719     C                   add       1             giro
126300130611     C     vidsun        chain(e)  sgius
126400130611     C                   if        %found(sgius)
126500130611     C                   eval      chk = '0'
126600130611     C                   endif
126700000719     C*
126800000719     C                   if        chk = '1'
126900000719     C                   movel     MSG(21)       DSMSMS                         *MESSAGGIO
127000000719     C                   endif
127100000719     C*
127200000719     C                   close     sgius
127300000719     C*
127400010605     C                   ENDSR
127500991008     C*------------------------------------------------------------------------*
127600000807     C* Cntisv - CONTROLLA codice tipo servizio a cui il cliente è abilitato
127700991008     C*------------------------------------------------------------------------*
127800001116     C     cntisv        begsr
127900001116     C                   clear                   TIBS02DS
128000001116     C*
128100991104     C     '?'           SCAN      Vidisv                                 30
128200001116     C*  Ricerca
128300001116     C                   eval      t02tla = 'L'
128400001116    1C                   IF        *IN30 = *ON
128500001116     C                   movel     'R'           t02mod
128600001116     C                   else
128700001116     C                   movel     'C'           t02mod
128800001116     C                   movel(p)  vidisv        t02ke1
128900001116     C                   end
129000001116     C                   movel     'ISV'         t02cod
129100001116     C                   movel     knsif         t02sif
129200991008     C                   CALL      'TIBS02R'
129300001116     C                   parm                    KPJBA
129400001116     C                   parm                    TIBS02DS
129500001116     C
129600001116     C*
129700991008    2C                   IF        T02err = *blanks
129800991104     C                   movel     T02ke1        Vidisv
129900020802     C                   movel(p)  T02uni        DSISV
130000020802     C                   movel     §isvdesc      vddisv
130100001116     C                   else
130200001116     C                   movel     '1'           errl00
130300001116     C                   seton                                        62
130400991104     C                   MOVEL     MSG(9)        DSMSMS
130500991008     C                   ENDIF
130600001116     C
130700001116     C                   endsr
130800000807     C*------------------------------------------------------------------------*
130900991104     C* Cnttle - CONTROLLA codice tipo legame
131000991104     C*------------------------------------------------------------------------*
131100001116     C     cnttle        begsr
131200001116     C                   clear                   TIBS02DS
131300001116     C*
131400991104     C     '?'           SCAN      Vidtle                                 30
131500001116     C*  Ricerca
131600001116     C                   eval      t02tla = 'L'
131700991104    1C                   IF        *IN30 = *ON
131800001116     C                   movel     'R'           t02mod
131900001116     C                   else
132000001116     C                   movel     'C'           t02mod
132100001116     C                   movel(p)  vidtle        t02ke1
132200001116     C                   end
132300991104     C                   movel     'TLE'         t02cod
132400001116     C                   movel     knsif         t02sif
132500001116     C                   CALL      'TIBS02R'
132600001116     C                   parm                    KPJBA
132700001116     C                   parm                    TIBS02DS
132800001116     C
132900001116     C*
133000991104    2C                   IF        T02err = *blanks
133100000807     C                   exsr      chkaut
133200000807     C                   if        errl00 = *blanks
133300991104     C                   movel     T02ke1        Vidtle
133400091027     C                   movel     T02uni        dstle
133500991104     C                   movel     §tledesc      Vddtle
133600000807     C                   endif
133700000807     C                   else
133800000807     C                   movel     '1'           errl00
133900000807     C                   seton                                        66
134000000807     C                   MOVEL     MSG(12)       DSMSMS
134100991104     C                   ENDIF
134200000807     C
134300000807     C                   endsr
134400000807     C*------------------------------------------------------------------------*
134500000807     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
134600000807     C*------------------------------------------------------------------------*
134700000807     C     CHKAUT        BEGSR
134800000807     C*
134900020903     C* VERIFICO CHE L'UTENTE CHE EFFETTUA LA RICHIESTA SIA UN EDP*
135000020903     C                   IF        %subst(KNMUS:1:3) = 'EDP'
135100020903     C                   ELSE
135200020903     C                   MOVEL     '1'           ERRL00
135300020903     C                   MOVEL     MSG(23)       DSMSMS
135400020903     C                   ENDIF
135500000807     C*
135600000807     C                   ENDSR
135700970528     C*------------------------------------------------------------------------*
135800991105     C* CHKksu - CONTROLLA codice padre figlio
135900970528     C*------------------------------------------------------------------------*
136000991105     C     CHKksu        BEGSR
136100970528     C*
136200991105     C     *LIKE         DEFINE    VIDksu        Wksu
136300991105     C     *LIKE         DEFINE    VDDksu        WDksu
136400001116     C                   MOVE      *zeros        Wksc
136500021114     C                   MOVEL     'S'           flgKSU            1
136600970528     C*
136700970528     C* RICHIESTO ELENCO
136800970528     C                   SETOFF                                           28
136900991105     C     '?'           SCAN      Wksu                                   28
137000970528IF  1C     *IN28         IFEQ      *ON
137100991008     C* RICHIESTO ELENCO CODICI
137200021114     C                   MOVEL     'N'           flgKSU
137300001116     C* CLIENTI
137400991008     C                   CLEAR                   DSXC
137500020924     C                   Z-ADD     DUTKCI        DXCCAP
137600991008     C                   CALL      'XCLIR'
137700991008     C                   PARM                    DSXC
137800991008     C* SCOMPATTA
137900991008     C                   MOVEA     DXCKSC        KSA                        ALF
138000991008DO  2C     1             DO        30            J                 3 0
138100991008     C                   MOVE      KSA(J)        DSKSA                      F
138200991008     C                   Z-ADD     DSKSN         KSC(J)                     M
138300991008E   2C                   ENDDO
138400991011     C                   MOVEL     KSC(1)        WKSC              7
138500060807     C                   MOVEL     *all'0'       WKSU
138600060807     C                   MOVE      WKSC          WKSU
138700991011     C                   MOVEL     '1'           $TESVD            1        TA
138800060807E   1C                   ENDIF
138900991008     C*
139000991008     C* CODICE CLIENTE NUMERICO
139100021114IF  1C     WKSC          IFLT      *ZEROS
139200021114     C                   MOVEL     '1'           WERR              1
139300021114X   1C                   ELSE
139400021114     C*
139500970528     C* CONTROLLA ESISTENZA CODICE
139600021114     C                   MOVEL     'S'           flgKSU
139700991210     C                   MOVE      Wksu          wcli              7 0
139800001116     C                   EXSR      dectib
139900021114E   1C                   ENDIF
140000970528     C*
140100970528     C                   ENDSR
140200991210     C*------------------------------------------------------------------------*
140300001116     C* DECTIB - Controlla esistenza codice cliente
140400991210     C*------------------------------------------------------------------------*
140500001116     C     DECTIB        BEGSR
140600001116     C*
140700010913     C     VIDksu        CHAIN     tianv01l
140800010913     C                   IF        %found(tianv01l)
140900010913     C                   MOVEL(p)  anvdes        WDKsu
141000010913     C                   ELSE
141100050909     C*
141200050909     C* SOLO CARATTERI NUMERICI
141300050909     C   28              MOVE      *all'0'       wksu
141400050909     C   28              MOVE      wksc          wksu
141500050909     C                   SETOFF                                       28
141600050909     C                   TESTN                   Wksu                 28
141700050909IF  2C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
141800120327     C                   MOVEL     '1'           ERRL00
141900050909X   2C                   ELSE                                                   *E'NUMERO DA TESTN
142000050909     C                   MOVE      Wksu          W001              1
142100050909IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
142200050909     C                   MOVEL     '1'           ERRL00
142300050909X   3C                   ELSE                                                   *NUMERO
142400991210     C                   CLEAR                   BS69DS
142500991210     C                   CLEAR                   ACODS
142600991210     C                   MOVEL     KNSIF         I69SIF
142700991210     C                   Z-ADD     WCLI          I69KAC
142800991210     C                   CALL      'TIBS69R'
142900991210     C                   PARM                    BS69DS
143000991210     C                   PARM                    ACODS
143100991210     C                   PARM                    INDDS
143200991210     C                   PARM                    CLPDS
143300991210     C                   PARM                    CLSDS
143400991210     C     O69ERR        IFNE      '1'
143500991210     C                   MOVEL     ACORAG        wdksu
143600991210     C                   ELSE
143700000728     C                   MOVEL     *ALL'*'       Wdksu
143800001116     C                   MOVE      '1'           errl00
143900021114     C                   MOVEL     'N'           flgKSU
144000001116     C                   ENDIF
144100050909E   3C                   ENDIF
144200050909E   2C                   ENDIF
144300991210     C                   ENDIF
144400001116     C*
144500001116     C                   ENDSR
144600991210     C*------------------------------------------------------------------------*
144700991210     C* FINSR - OPERAZIONI FINALI
144800991210     C*------------------------------------------------------------------------*
144900991210     C     FINSR         BEGSR
145000991210     C*
145100991210     C* CHIUDE I FILE DEI PGM CHIAMATI
145200991210     C                   CLEAR                   bs69ds
145300991210     C                   CLEAR                   acods
145400991210     C                   CLEAR                   indds
145500991210     C                   CLEAR                   clpds
145600991210     C                   CLEAR                   clsds
145700991210     C                   MOVEL     'C'           i69tla
145800991210     C                   MOVEL     knsif         i69sif
145900991210     C                   CALL      'TIBS69R'
146000991210     C                   PARM                    bs69ds
146100991210     C                   PARM                    acods
146200991210     C                   PARM                    indds
146300991210     C                   PARM                    clpds
146400991210     C                   PARM                    clsds
146500991210     C*
146600991210     C                   ENDSR
146700970528     C*------------------------------------------------------------------------*
146800970528     C* CHKDA8 - CONTROLLA LA DATA (8)
146900970528     C*------------------------------------------------------------------------*
147000970528     C     CHKDA8        BEGSR
147100970528     C*
147200970528     C     *LIKE         DEFINE    VIDDDE        WDA8
147300970528     C     *LIKE         DEFINE    VIDDDE        DA8W
147400970528     C*
147500970528     C                   Z-ADD     WDA8          G08DAT
147600970528     C                   Z-ADD     *ZEROS        G08INV
147700970528     C                   MOVEL     '0'           G08ERR
147800970528     C                   CALL      'XSRDA8'
147900970528     C                   PARM                    WLBDA8
148000970528     C                   Z-ADD     G08DAT        WDA8
148100970528     C                   Z-ADD     G08INV        DA8W
148200970528IF  1C     G08ERR        IFEQ      '1'
148300970528     C                   MOVEL     '1'           ERRL00
148400970528E   1C                   ENDIF
148500970528     C*
148600970528     C                   ENDSR
148700970526     C*------------------------------------------------------------------------*
148800970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
148900970526     C*------------------------------------------------------------------------*
149000970526     C     IREVID        BEGSR
149100970526     C* CAMPI
149200991104     C                   MOVEL     vsstle        VIDtle
149300991104     C                   MOVEL     vssisv        VIDisv
149400991104     C                   MOVEL     vssazp        VIDazp
149500991105     C                   MOVEL     vsssun        VIDsun
149600001116     C                   MOVE      vssksu        VIDksu
149700090804     C                   MOVEL     vssDTA        DVSSDTA
149800090804     C                   MOVEL     §VSSSUBK      VIDSUBK
149900991104     C                   Z-ADD     vssDDE        G08INV
150000970528     C                   Z-ADD     *ZEROS        G08DAT
150100970528     C                   MOVEL     '3'           G08ERR
150200970528     C                   CALL      'XSRDA8'
150300970528     C                   PARM                    WLBDA8
150400970528     C                   Z-ADD     G08DAT        VIDDDE
150500991104     C                   Z-ADD     vssDSC        G08INV
150600970528     C                   Z-ADD     *ZEROS        G08DAT
150700970528     C                   MOVEL     '3'           G08ERR
150800970528     C                   CALL      'XSRDA8'
150900970528     C                   PARM                    WLBDA8
151000051013     C                   Z-ADD     G08DAT        VIDDSC
151100051013     C*
151200051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
151300051013     C                   IF        DEPOP1 = 'F06' OR
151400051013     C                             DEPOP1 = 'O03' OR
151500051013     C                             DEPOP1 = 'O07'
151600051013     C                   EVAL      VIDDDE = DATCORINV
151700060726     C                   EVAL      VIDDSC = 31122039
151800051013     C                   ENDIF
151900970527     C*
152000970526     C                   ENDSR
152100970526     C*------------------------------------------------------------------------*
152200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
152300970526     C*------------------------------------------------------------------------*
152400970526     C     AZZVID        BEGSR
152500970526     C* CAMPI
152600000316     C*---               MOVEL     *BLANKS       VIDtle
152700991104     C                   MOVEL     *BLANKS       VIDisv
152800100913     C                   MOVEL     *BLANKS       VIDpwdftp
152900000316     C*---               MOVEL     *BLANKS       VIDazp
153000000316     C*---               Z-ADD     *ZEROS        VIDDDE
153100000316     C*---               Z-ADD     *ZEROS        VIDDSC
153200000316     C*---               move      *ZEROS        VIDksu
153300000316     C*---               move      *ZEROS        VIDsun
153400970526     C* DECODIFICHE
153500000316     C*---               MOVEL     *BLANKS       VDDtle
153600000215     C                   MOVEL     *BLANKS       VIDmsg
153700000316     C*---               MOVEL     *BLANKS       VDDksu
153800000316     C                   MOVEL     *BLANKS       VDDisv
153900970526     C*
154000970526     C                   ENDSR
154100970526     C*------------------------------------------------------------------------*
154200991104     C* IMPvss - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
154300970526     C*------------------------------------------------------------------------*
154400991104     C     IMPvss        BEGSR
154500970526     C*
154600991104     C                   MOVE      VIDazp        vssazp                         *CHIAVE
154700001116     C                   MOVE      ksuVID        vssksu
154800090804     C                   MOVEL     VIDSUBK       §VSSSUBK
154900090831     C                   EVAL      vssDTA = DVSSDTA
155000001116     C                   MOVE      VIDtle        vsstle
155100090714     C                   MOVE      VIDisv        vssISV
155200991104     C                   Z-ADD     DDEVID        vssDDE
155300991104     C                   Z-ADD     DSCVID        vssDSC
155400970526     C*
155500970526     C                   ENDSR
155600010605     C*------------------------------------------------------------------------*
155700010605     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE DELLE ABILITAZIONI CLIENTI/TIPI FILE (TIVIR)
155800010605     C*------------------------------------------------------------------------*
155900010605     C     IMPvir        BEGSR
156000010605     C*
156100010605     C                   MOVE      ksuVID        virksc
156200010605     C                   MOVE      'TO'          virtip
156300010605     C                   MOVE      'TT'          virfi1
156400010605     C                   MOVE      001           virseq
156500010605     C                   MOVE      999           virpoi
156600010605     C                   EVAL      virdes = 'ORM DA INTERNET'
156700010605     C                   MOVEL     'G '          virfi2
156800010605     C                   Z-ADD     DDEVID        virdti
156900010605     C                   Z-ADD     DSCVID        virdtf
157000010605     C*
157100010605     C                   ENDSR
157200970526     C*------------------------------------------------------------------------*
157300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
157400970526     C*------------------------------------------------------------------------*
157500970526     C     IMPFUN        BEGSR
157600970526     C*
157700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
157800970526IF  1C     DEPOP1        IFEQ      'O05'
157900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
158000970526     C                   MOVEL     'N'           FUDATT
158100970526     C                   MOVEL     FUDRIG        FUD(6)
158200970526E   1C                   ENDIF
158300100914     C*
158400100914     C* SE NO INSERIMENTO e NO COPIOA => NO "F18"
158500100914     C                   IF        DEPOP1 <> 'F06' AND
158600100914     C                             DEPOP1 <> 'O03'
158700111216     C* NO F18 anche se l'AS400 non è SETRAS
158800111216     C                             or  AS400_ok = *off
158900100914     C                   MOVEL     FUD(18)       FUDRIG                         *AGGIORNAMENTO
159000100914     C                   MOVEL     'N'           FUDATT
159100100914     C                   MOVEL     FUDRIG        FUD(18)
159200100914     C                   ENDIF
159300970526     C*
159400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
159500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
159600970526     C                   EXSR      FUNGES
159700970526     C*
159800970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
159900970526     C*---
160000970526     C* 90 -> PROTEZIONE CHIAVE
160100970526     C* 91 -> PROTEZIONE TOTALE
160200030704     C* 92 -> PROTEZIONE PARZIALE
160300970526     C*---
160400970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
160500970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
160600970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
160700970526     C                   SETOFF                                       91
160800970526E   2C                   ENDIF
160900970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
161000970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
161100970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
161200970526E   2C                   ENDIF
161300970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
161400970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
161500970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
161600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
161700970526     C                   SETON                                        91
161800970526E   2C                   ENDIF
161900970526X   1C                   ELSE                                                   *I N T E R R O G A
162000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
162100970526     C                   SETON                                        91
162200970526E   1C                   ENDIF
162300030704     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
162400030704     C                   SETON                                        92        *SPROTEZIONE SCAD.
162500030704     C                   ENDIF
162600970526     C*
162700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
162800970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
162900991104     C                   MOVEL     s20isv        KCEisv
163000001116     C                   MOVE      s20sun        KCEsun
163100991104     C     KEYvss        CHAIN     tivss01L                           9998
163200001116IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
163300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
163400970526     C                   MOVEL     MSG(6)        DSMSMS
163500970526X   2C                   ELSE
163600970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
163700980128IF  3C     DEPOP1        IFEQ      'O05'
163800001116     C     DEPOP1        OREQ      'O02'
163900101022     C*
164000101022     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
164100160705     C     vssISV        lookup    isvftp                                 80
164200160705     C                   if        %subst(KNMUS:1:3) <> 'EDP'
164300160705     C                   setoff                                       80
164400160705     C                   endif
164500101022     C                   IF        *IN80
164600111216     C                             and AS400_ok = *on
164700101022      /FREE
164800101022
164900101022         if UBBVSQL01_RtvUtente(
165000101022                         pInConn
165100101022                       : %subst(vssKSU:2:7)
165200101022                       : pOutNumRec
165300101022                       : pOutUsrPwd
165400101022                       : pOutUsrPath
165500101022                       : pOutUsrDesc
165600101022                       : pOutUsrFAnn
165700101022                       : pOutUsrEmail
165800101022                       : pOutUsrNote
165900101022                       : pOutUsrFList) = 0;
166000101022              if pOutNumRec = 1;
166100101022                 VIDPWDFTP = pOutUsrPwd;
166200101022              endif;
166300101022         endif;
166400101022
166500101022      /END-FREE
166600101022     C
166700101022     C                   ENDIF
166800101022     C*
166900101022     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
167000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
167100980128E   3C                   ENDIF
167200970526E   2C                   ENDIF
167300970526X   1C                   ELSE                                                   *INSERIMENTO
167400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
167500970526E   1C                   ENDIF
167600970526     C*
167700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
167800970526IF  1C     DEPOP1        IFEQ      'O02'
167900970526     C                   MOVEL     DEC(1)        VIDFUN
168000970526E   1C                   ENDIF
168100991014IF  1C     DEPOP1        IFEQ      'O03'
168200991014     C                   MOVEL     DEC(6)        VIDFUN
168300991014E   1C                   ENDIF
168400970526IF  1C     DEPOP1        IFEQ      'O04'
168500970526     C                   MOVEL     DEC(2)        VIDFUN
168600970526E   1C                   ENDIF
168700970526IF  1C     DEPOP1        IFEQ      'O05'
168800970526     C                   MOVEL     DEC(3)        VIDFUN
168900970526E   1C                   ENDIF
169000970526IF  1C     DEPOP1        IFEQ      'O07'
169100970526     C                   MOVEL     DEC(4)        VIDFUN
169200970526E   1C                   ENDIF
169300970526IF  1C     DEPOP1        IFEQ      'F06'
169400970526     C                   MOVEL     DEC(5)        VIDFUN
169500970526E   1C                   ENDIF
169600970526     C*
169700970526     C                   ENDSR
169800020212     C*--------------------------------------------------------------------------------------------*
169900020212     C* CARTAB - carica le tabelle occorrenti
170000020212     C*--------------------------------------------------------------------------------------------*
170100020212     C     CARTAB        BEGSR
170200020212     C*
170300020212     C* Tipi servizi soggetti a controllo unificazioni
170400020212     C                   z-add     *zeros        x                 3 0
170500050118     C                   z-add     *zeros        y                 3 0
170600100217     C                   z-add     *zeros        k                 3 0
170700100913     C                   z-add     *zeros        ww                3 0
170800020212     C                   eval      tbecod = 'ISV'
170900091027     C     tbecod        setll     tntbe01l
171000091027     C     tbecod        reade     tntbe01l
171100020212     C                   dow       not %eof(tntbe01l)
171200020212     C                   movel     tbeuni        dsisv
171300020212     C                   if        §isvuni  = 'S'
171400020212     C                   add       1             x
171500020212     C                   eval      isvkun(x) = tbeke1
171600020212     C                   endif
171700080110     C                   if        §isvrgr  = 'T'
171800050118     C                   add       1             y
171900050118     C                   eval      isvtt(y) = tbeke1
172000050118     C                   endif
172100100913     C                   if        §isvftp  = 'F'
172200100913     C                   add       1             ww
172300100913     C                   eval      isvftp(ww) = tbeke1
172400100913     C                   endif
172500100217     C                   add       1             k
172600100217     C                   eval      isvKEYS(k)   = tbeke1
172700100217     C                   eval      isvCTRL(k)   = §ISVCTRL
172800101006     C                   eval      isvPGMAFT(k) = §ISVPGMSRV
172900091027     C     tbecod        reade     tntbe01l
173000020212     C                   enddo
173100091027     c*---
173200091027     c* tipi legami "internet"
173300091027     c*---
173400091027     c                   z-add     *zeros        jtle
173500091027     c                   movel     'TLE'         tbecod
173600091027     c     tbecod        setll     tntbe01l
173700091027     c     tbecod        reade     tntbe01l
173800091027     c                   dow       not %eof(tntbe01l)
173900091027     c                   if        tbeatb = *blanks
174000091027     c                   eval      dstle = tbeuni
174100091027     c* solo legami d tipo "internet"
174200091027     c                   if        §TLEGRP = 'W'
174300091027     c                   add       1             jtle
174400091027     c                   eval      sktle(jtle) = tbeke1
174500091027     c                   endif
174600091027     c                   endif
174700091027     c     tbecod        reade     tntbe01l
174800091027     c                   enddo
174900020212     C*
175000020212     C                   ENDSR
175100020924     C*--------------------------------------------------------------------------------------------*
175200020924     C* REPERISCE I DATI UTENTE
175300020924     C*--------------------------------------------------------------------------------------------*
175400020924     C     REPDATIUTE    BEGSR
175500020924     C*
175600020924     C* INIZIALIZZA VARIABILI DI WRK
175700020924     C                   CLEAR                   TIBS34DS
175800020924     C                   CLEAR                   AZUTEDS
175900020924     C                   CLEAR                   DDATIUTE
176000020924     C*
176100020924     C     *DTAARA       DEFINE    §azute        azuteds
176200020924     C     *DTAARA       DEFINE    §datiute      ddatiute
176300020924     C                   IN(E)     *DTAARA
176400020924if  1C                   IF        %Error
176500020924     c                   EVAL      I34Tla = 'L'
176600020924     C                   CALL      'TIBS34R'
176700020924     C                   PARM                    Tibs34Ds
176800020924     C                   IN        *DTAARA
176900020924e   1C                   ENDIF
177000020924     C*
177100020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
177200020924     C                   MOVEL(P)  RSUT          DSFIRS           20
177300020924     C*
177400020924     C                   ENDSR
177500040422     C*------------------------------------------------------------------------*
177600040422     C* EXESGI - GESTIONE PARAMETRI PARTICOLARI X UTENTE STRATEGI
177700040422     C*------------------------------------------------------------------------*
177800040422     C     EXESGI        BEGSR
177900040422     C*
178000040422     C* Inizializzo il flag che indica l'autorizzazione speciale da attribuire al servizio
178100040422     C                   movel     *blanks       wABILSGI          1
178200040422     C*
178300040422     C* Innanzitutto verifico se il servizio è d tipo STRATEGI e quale autorizzazione speciale
178400040422     C* prevede
178500040422     C                   eval      KTBcod = 'ISV'
178600040422     C                   eval      KTBky1 = VIDISV
178700040422     C     KEYtbe        chain     tntbe01l
178800040422     C                   if        %found(tntbe01l)
178900040422     C                   eval      DSISV = tbeUNI
179000040422     C                   if        §ISVPROC = 'S' AND
179100040422     C                             §ISVSGI1 <> *blanks
179200040422     C                   eval      wABILSGI = §ISVSGI1
179300040422     C                   endif
179400040422     C                   endif
179500040422     C*
179600040422     C* Se x il servizio è prevista una particolare abilitazione => procedo
179700040422     C                   if        wABILSGI <> *blanks
179800040422     C* Costrisco la stringa d comando base x impostare i parametri adeguati sull'utente STRATEGI
179900040422     C                   eval      CMD = 'STRATEGI/CHGSGIUSR ' +
180000040422     C                                   VIDSUN + '  ' +
180100040422     C                                   'ALWWEB(' + api + '*YES' + api + ') '
180200040422     C*
180300040422     C* Gestisco i vari parametri diversi da abilitare a seconda del servizio
180400040422     C                   if        wABILSGI = 'U'                               * UPLOAD
180500040422     C                   eval      CMD = %trim(CMD) + ' ' +
180600040422     C                             'ALWRCVFILE(' + api + '*YES' + api + ') '
180700040422     C                   endif
180800040422     C                   if        wABILSGI = 'D'                               * DOWNLOAD
180900040422     C                   eval      CMD = %trim(CMD) + ' ' +
181000040422     C                                'ALWFILE(' + api + '*YES' + api + ') '
181100040422     C                   endif
181200040422     C                   if        wABILSGI = 'S'                               * OUTQ
181300040422     C                   eval      CMD = %trim(CMD) + ' ' +
181400040422     C                              ' ALWSPOOL(' + api + '*YES' + api + ') '
181500040422     C                   endif
181600040422     C*
181700040422     C* Esegue comando remoto su AS777
181800040422     C                   call(e)   'TIS730C'
181900040422     C                   parm                    cmd
182000040422     C*
182100040422     C                   endif
182200040422     C*
182300040422     C                   ENDSR
182400090610     C*------------------------------------------------------------------------*
182500090610     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
182600090610     C*------------------------------------------------------------------------*
182700090610     C     CHKSGI        BEGSR
182800090714      /FREE
182900090714               if UBUTISGI_Init = 0;
183000090714               endif;
183100090714      /END-FREE
183200090611     C*
183300090610      /FREE
183400090610               pInSUN = vidSUN;
183500090610               pInKSU = vidKSU;
183600090611               pInRGR = 'S';
183700090610
183800090610               if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
183900090610               endif;
184000090610
184100090610               if pOutEsito = '0';
184200090610                  if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
184300090610                  endif;
184400110503               else;
184500090611                  if UBUTISGI_EnableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
184600090611                  endif;
184700090611               endif;
184800090611      /END-FREE
184900090611     C
185000090923     C
185100090714      /FREE
185200090714               if UBUTISGI_Finalize = 0;
185300090714               endif;
185400090714      /END-FREE
185500090714     C*
185600090610     C                   ENDSR
185700101022
185800101022
185900101022
186000101022     c*------------------------------------------------------------------------*
186100101022     c* opnCONN - Apertura connessione
186200101022     c*------------------------------------------------------------------------*
186300101022     c     OPNCONN       BEGSR
186400101022      /FREE
186500101022
186600101022        if not *in40;
186700101022           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
186800101022              pInConn = pOutConn;
186900101022           else;
187000101022              *in40 = *on;
187100101022              s725_§21_ERRFLG = 'E';
187200101022              s725_§21_ERRMSG = 'Errore BartVAS: fatal error ';
187300101022           endif;
187400101022        endif;
187500101022
187600101022      /END-FREE
187700101022      *
187800101022     c                   ENDSR
187900101022     c*------------------------------------------------------------------------*
188000101022
188100101022
188200101022
188300101022     c*------------------------------------------------------------------------*
188400101022     c* clsCONN - Chiusura connessione
188500101022     c*------------------------------------------------------------------------*
188600101022     c     CLSCONN       BEGSR
188700101022      /FREE
188800101022
188900101022           if UBBVSQL01_Disconnect(pInConn) = 0;
189000101022           else;
189100101022              *in40 = *on;
189200101022           endif;
189300101022
189400101022      /END-FREE
189500101022      *
189600101022     c                   ENDSR
189700101022     c*------------------------------------------------------------------------*
189800101022
189900101022
190000101022
190100970526     C*------------------------------------------------------------------------*
190200970526     C* *INZSR - OPERAZIONI INIZIALI
190300970526     C*------------------------------------------------------------------------*
190400970526     C     *INZSR        BEGSR
190500970526     C*---
190600970526     C* RICEVIMENTO PARAMETRI
190700970526     C*---
190800970526     C     *ENTRY        PLIST
190900970612     C                   PARM                    KPJBA
191000001116     C                   MOVEL     KPJBU         tis720
191100020924     C*
191200020924     C* REPERISCE I DATI DELL'UTENTE
191300020924     C                   EXSR      REPDATIUTE
191400970526     C*
191500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
191600970526     C     XMSGPA        PLIST
191700970526     C                   PARM                    DSMSMS
191800970526     C                   PARM                    DSMSPG
191900970526     C                   PARM                    DSMSKE
192000970526     C                   PARM                    DSMSER
192100970526     C*
192200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
192300970526     C                   CALL      'XKEY'
192400970526     C                   PARM                    DSKEY
192500020212     C*
192600020212     C* CARICA LE TABELLE OCCORRENTI
192700020212     C                   EXSR      CARTAB
192800111216     C*
192900111216     C* Reperisco il sistema AS/400 corrente
193000111216     C                   EVAL      AS400_ok = *off
193100111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
193200111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
193300111216     C                   EVAL      AS400_ok = *on
193400111216     C                   ENDIF
193500101022     C*
193600101022     C* Apro la connessione al DB "BARTVAS"
193700111216     C                   IF        AS400_ok = *on
193800101022     C                   exsr      opnCONN
193900111216     C                   ENDIF
194000001116     C*---
194100001116     C* VARIABILI NON RIFERITE AL DATA BASE
194200001116     C*---
194300001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
194400001116     C     *LIKE         DEFINE    s20OP0        DEPOP0
194500001116     C     *LIKE         DEFINE    s20OP1        DEPOP1
194600001116     C     *LIKE         DEFINE    s20OP0        WOP0
194700001116     C     *LIKE         DEFINE    s20OP1        WOP1
194800001116     C*---
194900001116     C* VARIABILI RIFERITE AL DATA BASE
195000001116     C*---
195100001116     C     *LIKE         DEFINE    vssisv        KCEisv
195200001116     C     *LIKE         DEFINE    vsssun        KCEsun
195300001116     C     *LIKE         DEFINE    vssksu        KCEksu
195400001116     C     *LIKE         DEFINE    kuntle        KKUtle
195500001116     C     *LIKE         DEFINE    kunazp        KKUazp
195600001116     C     *LIKE         DEFINE    kuncop        KKUcop
195700020211     C     *LIKE         DEFINE    kuncof        KKUcof
195800020211     C     *LIKE         DEFINE    kundde        KKUdde
195900001116     C     *LIKE         DEFINE    tbecod        KTBcod
196000001116     C     *LIKE         DEFINE    tbeke1        KTBky1
196100010605     C     *LIKE         DEFINE    virtip        KCEtip
196200010605     C     *LIKE         DEFINE    virksc        KCEksc
196300010605     C     *LIKE         DEFINE    virdti        KCEdti
196400001116     C*---
196500001116     C* VARIABILI DI CONTROLLO
196600001116     C*---
196700001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
196800001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
196900001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
197000001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
197100010608     C                   MOVEL     '0'           ERRL01            1            *ERRORE (x TIVIR)
197200001116     C*---
197300001116     C* CHIAVI DI LETTURA
197400001116     C*---
197500001116     C* LETTURA tivss01L - COMPLETA
197600001116     C     KEYvss        KLIST
197700001116     C                   KFLD                    KCEsun                         *UTENTE STRATEGI
197800001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
197900100217     C* LETTURA tivss02L - PARZIALE
198000001116     C     KE2vss        KLIST
198100001116     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
198200001116     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
198300100217     C* LETTURA tivss02L - COMPLETA
198400100217     C     KEYvss02_C    KLIST
198500100217     C                   KFLD                    KCEksu                         *CLIENTE UNIFICANTE
198600100217     C                   KFLD                    KCEisv                         *TIPO SERVIZIO
198700100217     C                   KFLD                    KCEsun                         *STRATEGI USER NBR
198800001116     C* LETTURA tikun01L
198900001116     C     KEYkun        KLIST
199000001116     C                   KFLD                    KKUtle                         *TIPO LEGAME
199100001116     C                   KFLD                    KKUazp                         *AZIENDA PADRE
199200001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
199300001116     C* LETTURA tikun03L
199400001116     C     KE3kun        KLIST
199500001116     C                   KFLD                    KKUcop                         *CODICE CLI PADRE
199600020211     C* LETTURA tikun06L
199700020211     C     KE6kun        KLIST
199800020211     C                   KFLD                    KKUcof                         *CODICE CLI FIGLIO
199900020212     C                   KFLD                    KKUtle                         *TIPO LEGAME
200000001116     C* LETTURA tntbe01l
200100001116     C     KEYtbe        KLIST
200200001116     C                   KFLD                    KTBcod                         *CODICE TABELLA
200300001116     C                   KFLD                    KTBky1                         *CHIAVE 1 TABELLA
200400010605     C* LETTURA tivir03L - COMPLETA
200500010605     C     KEYvi2        KLIST
200600010605     C                   KFLD                    KCEtip                         *TIPO FILE
200700010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
200800010605     C                   KFLD                    KCEdti                         *DATA INIZIO ABIL.
200900010605     C* LETTURA tivir03L - INCOMPLETA
201000010605     C     KEYvir        KLIST
201100010605     C                   KFLD                    KCEtip                         *TIPO FILE
201200010605     C                   KFLD                    KCEksc                         *CODICE CLIENTE
201300970526     C*
201400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
201500991104     C                   MOVEL     'TIS725R'     DSMSPG
201600991104     C                   MOVEL     'TIS725R'     MSGQUE
201700970526     C                   MOVEL     *BLANKS       MSGKEY
201800970526     C                   MOVEL     *BLANKS       DSMSMS
201900970528     C*
202000970528     C* CALCOLA LA DATA CORRENTE
202100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
202200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
202300970528     C                   Z-ADD     WN8           G08DAT
202400051013     C                   Z-ADD     WN8           DATCORINV         8 0
202500970528     C                   Z-ADD     *ZEROS        G08INV
202600970528     C                   MOVEL     '0'           G08ERR
202700970528     C                   CALL      'XSRDA8'
202800970528     C                   PARM                    WLBDA8
202900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
203000970526     C*
203100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
203200991104     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
203300991104     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
203400051013     C*
203500051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
203600051013     C                   IF        DEPOP1 = 'F06' OR
203700051013     C                             DEPOP1 = 'O03' OR
203800051013     C                             DEPOP1 = 'O07'
203900051013     C                   EVAL      VIDDDE = DATCORINV
204000060726     C                   EVAL      VIDDSC = 31122039
204100051013     C                   ENDIF
204200101022     C                   IF        DEPOP1 = 'F06' OR
204300101022     C                             DEPOP1 = 'O03'
204400101022     C                   SETOFF                                       81
204500101022     C                   ELSE
204600101022     C                   SETON                                        81
204700101022     C                   ENDIF
204800970526     C*
204900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
205000970526     C                   EXSR      IMPFUN
205100970526     C*
205200000131     C* ESEGUE SUBITO "DECODI" SE ENTRATI IN VISUALIZZAZIONE O MODIFICA
205300000915     C*    DEPOP1        IFEQ      'O05'
205400000915     C*    DEPOP1        OREQ      'O02'
205500000915     C*                  EXSR      DECODI
205600000915     C*                  ENDIF
205700000131     C*
205800970526     C                   ENDSR
205900970526     C*------------------------------------------------------------------------*
206000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
206100970526     C*------------------------------------------------------------------------*
206200970526     C     FUNGES        BEGSR
206300970526     C*
206400970526     C* PULISCO CAMPI DI LAVORO
206500970526     C                   Z-ADD     0             W                 5 0
206600970526     C*
206700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
206800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
206900970526     C     1             DO        24            I
207000970526     C                   MOVEL     FUD(I)        FUDRIG
207100970526     C*
207200970526     C* PULIZIA FUNZIONI PRECEDENTI
207300970526     C                   MOVEL     *BLANKS       FUV(I)
207400970526     C*
207500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
207600970526     C* (SALTA F24)
207700970526     C     FUDATT        IFEQ      'S'
207800970526     C     FUDFUN        ANDNE     '24'
207900970526     C                   ADD       1             W
208000970526     C                   MOVEL     FUDDES        FUV(W)
208100970526     C                   END
208200970526     C                   END                                                    DO WHILE
208300970526     C*
208400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
208500970526     C* ALTRIMENTI DISATTIVO F24
208600970526     C     1             DO        24            I
208700970526     C                   MOVEL     FUD(I)        FUDRIG
208800970526     C*
208900970526     C     FUDFUN        IFEQ      '24'
209000970526     C     W             IFGT      6
209100970526     C                   MOVEL     'S'           FUDATT
209200970526     C                   ELSE
209300970526     C                   MOVEL     *BLANKS       FUDATT
209400970526     C                   END                                                    W GT 8
209500970526     C                   MOVEL     FUDRIG        FUD(I)
209600970526     C                   END
209700970526     C                   END                                                    DO WHILE
209800970526     C*
209900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
210000970526     C     W             DIV       6             I
210100970526     C                   MVR                     Z
210200970526     C*
210300970526     C     Z             IFNE      0
210400970526     C     I             ADD       1             Z
210500970526     C                   ELSE
210600970526     C                   Z-ADD     I             Z                 5 0
210700970526     C                   END
210800970526     C*
210900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
211000970526     C                   ADD       1             FUNGRU
211100970526     C     FUNGRU        IFGT      Z
211200970526     C     FUNGRU        ORLT      1
211300970526     C                   Z-ADD     1             FUNGRU
211400970526     C                   END
211500970526     C*
211600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
211700970526     C     FUNGRU        IFEQ      1
211800970526     C                   MOVEL     FUV(1)        FUN001
211900970526     C                   MOVEL     FUV(2)        FUN002
212000970526     C                   MOVEL     FUV(3)        FUN003
212100970526     C                   MOVEL     FUV(4)        FUN004
212200970526     C                   MOVEL     FUV(5)        FUN005
212300970526     C                   MOVEL     FUV(6)        FUN006
212400970526     C                   END
212500970526     C*
212600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
212700970526     C     FUNGRU        IFEQ      2
212800970526     C                   MOVEL     FUV(7)        FUN001
212900970526     C                   MOVEL     FUV(8)        FUN002
213000970526     C                   MOVEL     FUV(9)        FUN003
213100970526     C                   MOVEL     FUV(10)       FUN004
213200970526     C                   MOVEL     FUV(11)       FUN005
213300970526     C                   MOVEL     FUV(12)       FUN006
213400970526     C                   END
213500970526     C*
213600970526     C* TERZO GRUPPO DI 6 FUNZIONI
213700970526     C     FUNGRU        IFEQ      3
213800970526     C                   MOVEL     FUV(13)       FUN001
213900970526     C                   MOVEL     FUV(14)       FUN002
214000970526     C                   MOVEL     FUV(15)       FUN003
214100970526     C                   MOVEL     FUV(16)       FUN004
214200970526     C                   MOVEL     FUV(17)       FUN005
214300970526     C                   MOVEL     FUV(18)       FUN006
214400970526     C                   END
214500970526     C*
214600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
214700970526     C     FUNGRU        IFEQ      4
214800970526     C                   MOVEL     FUV(19)       FUN001
214900970526     C                   MOVEL     FUV(20)       FUN002
215000970526     C                   MOVEL     FUV(21)       FUN003
215100970526     C                   MOVEL     FUV(22)       FUN004
215200970526     C                   MOVEL     *BLANKS       FUN005
215300970526     C                   MOVEL     *BLANKS       FUN006
215400970526     C                   END
215500970526     C*
215600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
215700970526     C                   MOVEL     *BLANKS       FUN007
215800970526     C                   MOVEL     *BLANKS       FUN008
215900970526     C*
216000970526     C     1             DO        24            I
216100970526     C                   MOVEL     FUD(I)        FUDRIG
216200970526     C     FUDFUN        IFEQ      '24'
216300970526     C     FUDATT        ANDEQ     'S'
216400970526     C                   MOVEL     FUDDES        FUN008
216500970526     C                   END
216600970526     C                   END                                                    DO
216700970526     C*
216800970526     C                   ENDSR
216900970526     C*------------------------------------------------------------------------*
217000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
217100970526     C*------------------------------------------------------------------------*
217200970526     C     FUNCON        BEGSR
217300970526     C*
217400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
217500970526     C                   EXSR      FUNVAR
217600970526     C*
217700970526     C* PULISCO CAMPI DI LAVORO
217800970526     C                   Z-ADD     0             I                 5 0
217900970526     C                   Z-ADD     0             W                 5 0
218000970526     C*
218100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
218200970526     C     FUNKEY        IFNE      *BLANKS
218300970526     C     1             DO        24            I
218400970526     C                   MOVEL     FUD(I)        FUDRIG
218500970526     C*
218600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
218700970526     C     FUNKEY        IFEQ      FUDFUN
218800970526     C     FUDATT        ANDEQ     'S'
218900970526     C                   Z-ADD     I             W
219000970526     C                   END
219100970526     C                   END                                                    DO WHILE
219200970526     C*
219300970526     C* FUNZIONE NON ATTIVA (ERRORE)
219400970526     C     W             IFEQ      0
219500970526     C*
219600970526     C* EMISSIONE MESSAGGIO DI ERRORE
219700970526     C                   MOVEL     MSG(1)        DSMSMS
219800970526     C*
219900970526     C                   END                                                    W=0
220000970526     C                   END                                                    FUNKEY NE *BLANKS
220100970526     C*
220200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
220300970526     C     W             IFNE      0
220400970526     C     FUNKEY        OREQ      *BLANKS
220500970526     C     FUNKEY        IFEQ      *BLANKS
220600970526     C                   EXSR      ENTGES
220700970526     C                   END
220800970526     C     FUNKEY        IFEQ      '03'
220900970526     C                   EXSR      F03GES
221000970526     C                   END
221100970526     C     FUNKEY        IFEQ      '06'
221200970526     C                   EXSR      F06GES
221300970526     C                   END
221400970526     C     FUNKEY        IFEQ      '12'
221500970526     C                   EXSR      F12GES
221600970526     C                   END
221700100914     C     FUNKEY        IFEQ      '18'
221800100914     C                   EXSR      F18GES
221900100914     C                   END
222000970526     C     FUNKEY        IFEQ      '24'
222100970526     C                   EXSR      F24GES
222200970526     C                   END
222300970526     C                   END
222400970526     C*
222500970526     C                   ENDSR
222600970526     C*------------------------------------------------------------------------*
222700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
222800970526     C*------------------------------------------------------------------------*
222900970526     C     FUNVAR        BEGSR
223000970526     C*
223100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
223200970526     C*
223300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
223400970526     C     KEY           IFEQ      ENTER
223500970526     C                   MOVEL     *BLANKS       FUNKEY
223600970526     C                   END
223700970526     C     KEY           IFEQ      F1
223800970526     C                   MOVEL     '01'          FUNKEY
223900970526     C                   END
224000970526     C     KEY           IFEQ      F2
224100970526     C                   MOVEL     '02'          FUNKEY
224200970526     C                   END
224300970526     C     KEY           IFEQ      F3
224400970526     C                   MOVEL     '03'          FUNKEY
224500970526     C                   END
224600970526     C     KEY           IFEQ      F4
224700970526     C                   MOVEL     '04'          FUNKEY
224800970526     C                   END
224900970526     C     KEY           IFEQ      F5
225000970526     C                   MOVEL     '05'          FUNKEY
225100970526     C                   END
225200970526     C     KEY           IFEQ      F6
225300970526     C                   MOVEL     '06'          FUNKEY
225400970526     C                   END
225500970526     C     KEY           IFEQ      F7
225600970526     C                   MOVEL     '07'          FUNKEY
225700970526     C                   END
225800970526     C     KEY           IFEQ      F8
225900970526     C                   MOVEL     '08'          FUNKEY
226000970526     C                   END
226100970526     C     KEY           IFEQ      F9
226200970526     C                   MOVEL     '09'          FUNKEY
226300970526     C                   END
226400970526     C     KEY           IFEQ      F10
226500970526     C                   MOVEL     '10'          FUNKEY
226600970526     C                   END
226700970526     C     KEY           IFEQ      F11
226800970526     C                   MOVEL     '11'          FUNKEY
226900970526     C                   END
227000970526     C     KEY           IFEQ      F12
227100970526     C                   MOVEL     '12'          FUNKEY
227200970526     C                   END
227300970526     C     KEY           IFEQ      F13
227400970526     C                   MOVEL     '13'          FUNKEY
227500970526     C                   END
227600970526     C     KEY           IFEQ      F14
227700970526     C                   MOVEL     '14'          FUNKEY
227800970526     C                   END
227900970526     C     KEY           IFEQ      F15
228000970526     C                   MOVEL     '15'          FUNKEY
228100970526     C                   END
228200970526     C     KEY           IFEQ      F16
228300970526     C                   MOVEL     '16'          FUNKEY
228400970526     C                   END
228500970526     C     KEY           IFEQ      F17
228600970526     C                   MOVEL     '17'          FUNKEY
228700970526     C                   END
228800970526     C     KEY           IFEQ      F18
228900970526     C                   MOVEL     '18'          FUNKEY
229000970526     C                   END
229100970526     C     KEY           IFEQ      F19
229200970526     C                   MOVEL     '19'          FUNKEY
229300970526     C                   END
229400970526     C     KEY           IFEQ      F20
229500970526     C                   MOVEL     '20'          FUNKEY
229600970526     C                   END
229700970526     C     KEY           IFEQ      F21
229800970526     C                   MOVEL     '21'          FUNKEY
229900970526     C                   END
230000970526     C     KEY           IFEQ      F22
230100970526     C                   MOVEL     '22'          FUNKEY
230200970526     C                   END
230300970526     C     KEY           IFEQ      F23
230400970526     C                   MOVEL     '23'          FUNKEY
230500970526     C                   END
230600970526     C     KEY           IFEQ      F24
230700970526     C                   MOVEL     '24'          FUNKEY
230800970526     C                   END
230900970526     C     KEY           IFEQ      ROLLUP
231000970526     C                   MOVEL     'RU'          FUNKEY
231100970526     C                   END
231200970526     C     KEY           IFEQ      HELP
231300970526     C                   MOVEL     'HP'          FUNKEY
231400970526     C                   END
231500970526     C                   ENDSR
231600970526     C*------------------------------------------------------------------------*
231700970526     C* NOTE COSTRUZIONE SCHIERE
231800970526     C*------------------------------------------------------------------------*
231900970526     C*
232000970526     C* -MSG-MESSAGGI
232100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
232200970526     C* (NON TOCCARE MSG,1 E MSG,2)
232300970526     C*
232400970526     C* -CMD-COMANDI
232500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
232600970526     C*
232700970526     C* -OPD-OPZIONI
232800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
232900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
233000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
233100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
233200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
233300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
233400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
233500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
233600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
233700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
233800970526     C*
233900970526     C* -FUD-FUNZIONI
234000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
234100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
234200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
234300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
234400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
234500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
234600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
234700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
234800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
234900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
235000970526     O*------------------------------------------------------------------------*
235100970526** DEC - DECODIFICHE
235200970526    MODIFICA                                                      1
235300970526  ANNULLAMENTO                                                    2
235400970526VISUALIZZAZIONE                                                   3
235500970526   RIPRISTINO                                                     4
235600970526  INSERIMENTO                                                     5
235700991105     COPIA                                                        6
235800000215** DES - DESCRIZIONI
235900000215ATTENZIONE: non esistono figli legati                             1
236000000215ATTENZIONE: esistono figli legati                                 2
236100020211                                                                  3
236200000215                                                                  4
236300000215                                                                  5
236400970526** MSG - MESSAGGI
236500991104TIS725R- Tasto funzionale non gestito                                          1
236600991104TIS725R- Inserimento effettuato                                                2
236700991104TIS725R- Modifica effettuata                                                   3
236800991104TIS725R- Annullamento effettuato                                               4
236900991104TIS725R- Ripristino effettuato                                                 5
237000991104TIS725R- Richiesta funzione per record inesistente                             6
237100991104TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
237200991104TIS725R- Richiesta funzione per record esistente                               8
237300991104TIS725R- Codice tipo servizio non presente in tabella                          9
237400000322TIS725R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
237500000317TIS725R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
237600091117TIS725R- Tipo legame inesistente o non utilizzabile                            12
237700991104TIS725R- Codice padre errato                                                   13
237800991104TIS725R- Data errata                                                           14
237900991104TIS725R- La data scadenza deve essere maggiore della decorrenza                15
238000991104TIS725R- Il record non può scadere in un giorno già trascorso                  16
238100991104TIS725R- Il record immesso si sovrappone ad uno già in decorrenza              17
238200991104TIS725R- Codice figlio errato                                                  18
238300000705TIS725R- Codice padre obbligatorio                                             19
238400991104TIS725R- Il codice figlio non può essere uguale al codice padre                20
238500000719TIS725R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
238600000807TIS725R- Tipo legame non autorizzato.                                          22
238700000807TIS725R- Filiale non abilitata alla gestione dei clienti scelti.               23
238800000913TIS725R- Servizio DL già presente nel periodo per il cliente scelto.           24
238900010605TIS725R- Nel periodo esistono già abilitazioni per il cliente al file TO.      25
239000010605TIS725R- Abilitazione per il cliente al file TO già esistente.                 26
239100020211TIS725R- Cliente già presente nei legami come figlio.                          27
239200020214TIS725R- Per il servizio indicato non è consentito specificare un legame.      28
239300020802TIS725R- Il codice Strategi è incompatibile con il servizio richiesto.         29
239400090804TIS725R- Campo valido solamente per servizi 'TT'                               30
239500100217TIS725R- E' necessaria una identica abilitazione al servizio 'TT'              31
239600100913TIS725R- Password necessaria x attivazione servizio richiesto.                 32
239700100913TIS725R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    33
239800100913TIS725R- Lunghezza minima password: 8 caratteri                                34
239900100914TIS725R- Tasto funzione non consentito per servizi diversi da 'IE'             35
240000101027TIS725R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       36
240100120327TIS725R- Cliente annullato o bloccato, impossibile abilitare EasyWeb           37
240200101027TIS725R-                                                                       38
240300101027TIS725R-                                                                       39
240400101027TIS725R-                                                                       40
240500970526** FUD - FUNZIONI
240600970526                   -        (S/N)=N                        funz.01¬
240700970526                   -        (S/N)=N                        funz.02¬
240800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
240900970526                   -        (S/N)=N                        funz.04¬
241000970526                   -        (S/N)=N                        funz.05¬
241100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
241200970527                   -        (S/N)=N                        funz.07¬
241300970527                   -        (S/N)=N                        funz.08¬
241400970527                   -        (S/N)=N                        funz.09¬
241500970527                   -        (S/N)=N                        funz.10¬
241600970526                   -        (S/N)=N                        funz.11¬
241700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
241800970526                   -        (S/N)=N                        funz.13¬
241900970526                   -        (S/N)=N                        funz.14¬
242000970526                   -        (S/N)=N                        funz.15¬
242100970526                   -        (S/N)=N                        funz.16¬
242200970526                   -        (S/N)=N                        funz.17¬
242300100914F18=Conf.EasySped  -        (S/N)=S                        funz.18¬
242400970526                   -        (S/N)=N                        funz.19¬
242500970526                   -        (S/N)=N                        funz.20¬
242600970526                   -        (S/N)=N                        funz.21¬
242700970526                   -        (S/N)=N                        funz.22¬
242800970526                   -        (S/N)=N                        funz.23¬
242900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
