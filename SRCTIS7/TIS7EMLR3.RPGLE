000100170116     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000200170116     H dftactgrp(*no) BNDDIR('TRBM') ACTGRP('BARTVAS')
000300000414
000400001120
000500060811     FTNTBE01L  IF   E           K DISK
000600010918     Ftiabl04l  UF   E           K DISK
000700010912
000800060811     D*------------------
000900010912     D* ARCHITETTURA
001000060811     D*------------------
001100010912     D KPJBA         E DS
001200060811     D*------------------
001300060811     D* VARIABILI DI WRK
001400060811     D*------------------
001500010919     D wrkCMD          S            200
001600060807     D contaMOD        S              5  0 inz(*zeros)
001700060822     D wrkEml          S            253    inz(*blanks)
001800060822     D wrkOgg          S             44    inz(*blanks)
001900060822     D wrkMsgCED       S           5000    inz(*blanks)
002000120627     D inTags          s          65535A   VARYING
002100170116     D string          s            200a   varying
002200170116     D Esito           s             10I 0
002300170116     D wMRAREG         s                   inz like(§MRAREG)
002400010919     D*------------
002500010912
002600120627      // Definizione prototipi procedure.
002700120627
002800120627     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
002900120627     D trul33r         PR
003000120627     D                                     EXTPGM('TRUL33R')
003100120627     D  kpjba
003200120627     D                                     LIKEDS(kpjba)
003300120627     d trul33ds      e ds
003400010912
003500000414     C*------------------------------------------------------------------------*
003600000414     C* MAIN LINE
003700000414     C*------------------------------------------------------------------------*
003800060811     C*
003900010919     C                   EXSR      SNDEML
004000060811     C*
004100060811     C* Infine mando email a ced con resoconto invii effettuati
004200120305     C                   eval      wrkEml = 'ced@brt.it'
004300060811     C                   eval      wrkOgg = 'Attivazione ai servizi ON LINE'
004400060811     C                   eval      wrkMsgCED = ':/N' + 'Inviate '              +
004500060811     C                                         %editc(contaMOD:'4')            +
004600060811     C                                         ' e-mail, segue dettaglio: '    +
004700060811     C                                         ':/N' + %trim(wrkMsgCED)
004800060811     C                   CALL(e)   'TIS701C'
004900060811     C                   PARM                    wrkEml
005000060811     C                   PARM                    wrkOgg
005100060811     C                   PARM                    wrkMsgCED
005200010912     C*
005300010912     C                   SETON                                        LR
005400120627     C*------------------------------------------------------------------------*
005500120627     C     REPMRABYPS    BEGSR
005600120627     C*
005700120627     C* Reperimento tabella MRABYPS
005800120627     C                   CLEAR                   InDMRABYPS
005900120627     C                   MOVE(P)   'MRA'         tbeCOD
006000120627     C                   MOVEL(p)  'TRBMBYPSR'   tbeKE1
006100170116     C                   MOVEL(p)  wMRAREG       tbeKE2
006200120627     C     KEYTBE01P     CHAIN     TNTBE01L
006300120627     C                   IF        %found(TNTBE01L) AND
006400120627     C                             tbeATB <> 'A'
006500120627     C                   SETOFF                                       60
006600120627     C                   MOVEL     tbeUNI        InDMRABYPS
006700120627     C                   ELSE
006800120627     C                   SETON                                        60
006900170116     C                   EVAL      wrkMsgCED = 'Record ' + wMRAREG +
007000170116     C                                         ' non trovato in tabella '+
007100170116     C                                         'MRABYPS, avvisare il CED.'
007200120627     C                   ENDIF
007300120627     C*
007400120627     C                   ENDSR
007500010912     C*------------------------------------------------------------------------*
007600170116     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
007700120627     C* Imposto campo contenente la stringa per driver TRBMBYPSR
007800010912     C*------------------------------------------------------------------------*
007900010912     C     SNDEML        BEGSR
008000060807     C*
008100060807     C                   movel     *blanks       wrkMsgCED
008200010912     C*
008300010918     C*  Mi posiziono all'inizio del file tiabl04l
008400010918     C     *loval        SETLL     tiabl04l
008500010912     C*  E scorro tutto il file
008600010918     C                   READ      tiabl04l
008700010918     C                   DOW       not %eof(tiabl04l)
008800010912     C*
008900010918     C* Se record è da inviare
009000010918     C                   IF        ablINV = '0'
009100170116     C*
009200170116     C                   EVAL      wMRAREG = *blanks
009300170116     C*
009400170116     C* A seconda del Tipo Testo abilitazione invio specifica email
009500170116     C                   SELECT
009600170116     C*
009700170116     C* Abilitazioni VAS - TT
009800170116     C                   WHEN      ablTIP = 'SG1'
009900170116     C                   EVAL      wMRAREG = 'AV1'
010000170116     C*
010100170116     C* Reperisco dati tabellati (Tabella 'MRABYPS')
010200170116     C                   EXSR      REPMRABYPS
010300170116     C*
010400170116     C                   IF        *IN60 = *OFF
010500060807     C*
010600060807     C* Compogo il messaggio da inviare a ced con il resoconto d quanto spedito
010700060809     C                   EVAL      wrkMsgCED = %trim(wrkMsgCED) + 'Cliente: '
010800060822     C                             +ablUTE+'   E-mail: '+%trim(ablEML)+':/N'
010900060811     C*
011000060811     C* Imposto quindi i dati utenti estesi
011100120627     C                   clear                   intrtcm1ds
011200170116     C                   EVAL      §CM1MITT = %trim(§MRAEMLMIT)
011300060811     C                   EVAL      §CM1VAR  = '*OBJM*' + 'Abilitazione ai ' +
011400060824     C                                        'servizi ON LINE ' + %trim(ablUTE)
011500170116     C                   EVAL      §CM1TIPS = §MRAREG
011600170116     C                   EVAL      §CM1IDP  = §MRAIDPRO
011700060811     C                   MOVEL(P)  '046'         §CM1PO
011800060811     C                   EVAL      §CM1DST  = ablEML
011900120627     C                   clear                   trul33ds
012000120627     C                   EVAL      i33Cnu = 24
012100120627     C                   EVAL      i33num = 1
012200120627     C                   EVAL      kpjbu = trul33ds
012300120627     C                   CALLP     trul33r(kpjba)
012400120627     C                   EVAL      trul33ds = kpjbu
012500120627     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
012600120627     C                   EVAL      §CM1ATT='01'
012700120627     C                   EVAL      §CM1TOTATT='01'
012800060811     C*
012900120627     C                   EVAL      inTags='§§Tag01§§=='+%trim(ablRSP)+'||'+
013000120627     C                                    '§§Tag02§§=='+%trim(ablUTE)+'||'+
013100120627     C                                    '§§Tag03§§=='+%trim(ablPWD)+'||'
013200120627     C*
013300120627     C                   EVAL      Esito = 0
013400120627     C*
013500120627     C                   CALLP     Bartmail_insert
013600120627     C                             (inTags:indmrabyps:intrtcm1ds:
013700120627     C                              %len(inTags):Esito)
013800060811     C*
013900120627     C                   IF        Esito = 0
014000120627     C*
014100010919     C                   EVAL      contaMOD = contaMOD + 1
014200010914     C* Dopo l'invio fleggo il record come "già inviato"
014300010919     C                   EVAL      ablINV = '1'
014400010919     C                   UPDATE    tiabl000
014500170116     C                   ENDIF
014600170116     C                   ENDIF
014700170116     C*
014800170116     C* Abilitazioni VAS - IW
014900170116     C                   WHEN      ablTIP = 'VAS'
015000170116     C                   EVAL      wMRAREG = 'AV2'
015100170116     C*
015200170116     C* Reperisco dati tabellati (Tabella 'MRABYPS')
015300170116     C                   EXSR      REPMRABYPS
015400170116     C*
015500170116     C                   IF        *IN60 = *OFF
015600170116     C*
015700170116     C* Imposto quindi i dati utenti estesi
015800170116     C                   clear                   intrtcm1ds
015900170116     C                   EVAL      §CM1MITT = %trim(§MRAEMLMIT)
016000170116     C                   EVAL      §CM1VAR  = '*OBJM*' + 'Abilitazione ai ' +
016100170116     C                                        'servizi ON LINE ' + %trim(ablUTE)
016200170116     C                   EVAL      §CM1TIPS = §MRAREG
016300170116     C                   EVAL      §CM1IDP  = §MRAIDPRO
016400170116     C                   MOVEL(P)  '046'         §CM1PO
016500170116     C                   EVAL      §CM1DST  = ablEML
016600170116     C                   clear                   trul33ds
016700170116     C                   EVAL      i33Cnu = 24
016800170116     C                   EVAL      i33num = 1
016900170116     C                   EVAL      kpjbu = trul33ds
017000170116     C                   CALLP     trul33r(kpjba)
017100170116     C                   EVAL      trul33ds = kpjbu
017200170116     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
017300170116     C                   EVAL      §CM1ATT='01'
017400170116     C                   EVAL      §CM1TOTATT='01'
017500170116     C*
017600170116     C                   EVAL      inTags='§§Tag01§§=='+%trim(ablRSP)+'||'+
017700170116     C                                    '§§Tag02§§=='+%trim(ablUTE)+'||'+
017800170116     C                                    '§§Tag03§§=='+%trim(ablPWD)+'||'
017900170116     C*
018000170116     C                   EVAL      Esito = 0
018100170116     C*
018200170116     C                   CALLP     Bartmail_insert
018300170116     C                             (inTags:indmrabyps:intrtcm1ds:
018400170116     C                              %len(inTags):Esito)
018500170116     C*
018600170116     C                   IF        Esito = 0
018700170116     C* Dopo l'invio fleggo il record come "già inviato"
018800170116     C                   EVAL      ablINV = '1'
018900170116     C                   UPDATE    tiabl000
019000170116     C                   ENDIF
019100010912     C                   ENDIF
019200170116     C*
019300170116     C                   ENDSL
019400170116     C*
019500060811     C                   ENDIF
019600060811     C*
019700010912     C* Proseguo con la lettura
019800010918     C                   READ      tiabl04l
019900010912     C*
020000010912     C                   ENDDO
020100010912     C*
020200060811     C                   ENDSR
020300120627     C*-------------------------------------------------------------*
020400120627     C* controllo stringa                                           *
020500120627     C*-------------------------------------------------------------*
020600120627     C     sr_chkstr     Begsr
020700120627     C                   EVAL      string=%xlate('||':'  ':string)
020800120627     C                   EVAL      string=%xlate('==':'  ':string)
020900120627     C                   endsr
021000010912     C*------------------------------------------------------------------------*
021100010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
021200010912     C*------------------------------------------------------------------------*
021300010912     C     *INZSR        BEGSR
021400010912     C*---
021500010912     C* RICEVIMENTO PARAMETRI
021600010912     C*---
021700010912     C     *ENTRY        PLIST
021800010912     C                   PARM                    KPJBA
021900060811     C*
022000060811     C* CHIAVI DI LETTURA
022100060811     C* TNTBE01L
022200060811     C     KEYTBE01P     KLIST
022300060811     C                   KFLD                    tbeCOD
022400060811     C                   KFLD                    tbeKE1
022500060811     C                   KFLD                    tbeKE2
022600010912     C*
022700010912     C                   ENDSR
