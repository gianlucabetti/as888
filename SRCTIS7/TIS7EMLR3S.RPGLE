000100000414
000200120627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300120627     h dftactgrp(*no) BNDDIR('TRBM') ACTGRP('BARTVAS')
000400000414
000500001120
000600060811     FTNTBE01L  IF   E           K DISK
000700010918     Ftiabl04l  UF   E           K DISK
000800010912
000900060811     D*------------------
001000010912     D* ARCHITETTURA
001100060811     D*------------------
001200010912     D KPJBA         E DS
001300060811     D*------------------
001400060811     D* TABELLEA 'MRA'
001500060811     D*------------------
001600120627     D DMRA          E DS                  prefix(X_)
001700060811     D*------------------
001800060811     D* DS ridefinizione dati utente estesi spool x mailing
001900060811     D*------------------
002000120627     D TRTCM1DS      E DS                  prefix(obs_)
002100060811     D*------------------
002200060811     D* VARIABILI DI WRK
002300060811     D*------------------
002400010919     D wrkCMD          S            200
002500060807     D contaMOD        S              5  0 inz(*zeros)
002600060822     D wrkEml          S            253    inz(*blanks)
002700060822     D wrkOgg          S             44    inz(*blanks)
002800060822     D wrkMsgCED       S           5000    inz(*blanks)
002900120627     D inTags          s          65535A   VARYING
003000120627     d string          s            200a   varying
003100120627     d Esito           s             10I 0
003200010919     D*------------
003300010912
003400120627      // Definizione prototipi procedure.
003500120627
003600120627     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
003700120627     D trul33r         PR
003800120627     D                                     EXTPGM('TRUL33R')
003900120627     D  kpjba
004000120627     D                                     LIKEDS(kpjba)
004100120627     d trul33ds      e ds
004200010912
004300000414     C*------------------------------------------------------------------------*
004400000414     C* MAIN LINE
004500000414     C*------------------------------------------------------------------------*
004600060811     C*
004700060811     C* Reperisco dati tabellati (Tabella 'MRA')
004800060811     C                   EXSR      REPMRA
004900120627     C*
005000120627     C* Reperisco dati tabellati (Tabella 'MRABYPS')
005100120627     C                   EXSR      REPMRABYPS
005200060811     C*
005300060811     C* Se tutto ok proseguo con l'effettiva elaborazione
005400060811     C                   IF        *IN60 = *OFF
005500010919     C                   EXSR      SNDEML
005600060811     C                   ENDIF
005700060811     C*
005800060811     C* Infine mando email a ced con resoconto invii effettuati
005900120305     C                   eval      wrkEml = 'ced@brt.it'
006000060811     C                   eval      wrkOgg = 'Attivazione ai servizi ON LINE'
006100060811     C                   eval      wrkMsgCED = ':/N' + 'Inviate '              +
006200060811     C                                         %editc(contaMOD:'4')            +
006300060811     C                                         ' e-mail, segue dettaglio: '    +
006400060811     C                                         ':/N' + %trim(wrkMsgCED)
006500060811     C                   CALL(e)   'TIS701C'
006600060811     C                   PARM                    wrkEml
006700060811     C                   PARM                    wrkOgg
006800060811     C                   PARM                    wrkMsgCED
006900010912     C*
007000010912     C                   SETON                                        LR
007100060811     C*------------------------------------------------------------------------*
007200060811     C     REPMRA        BEGSR
007300060811     C*
007400060811     C* Reperimento tabella MRA
007500060811     C                   CLEAR                   DMRA
007600060811     C                   MOVE(P)   'MRA'         tbeCOD
007700060811     C                   MOVEL(p)  'TIS7EMLR3'   tbeKE1
007800060811     C                   MOVE      *blanks       tbeKE2
007900060811     C     KEYTBE01P     CHAIN     TNTBE01L
008000060811     C                   IF        %found(TNTBE01L) AND
008100060811     C                             tbeATB <> 'A'
008200060811     C                   SETOFF                                       60
008300060811     C                   MOVEL     tbeUNI        DMRA
008400060811     C                   ELSE
008500060811     C                   SETON                                        60
008600080526     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR3 non trovato '+
008700080526     C                                      'in tabella MRA, avvisare il CED.'
008800060811     C                   ENDIF
008900060811     C*
009000060811     C                   ENDSR
009100120627     C*------------------------------------------------------------------------*
009200120627     C     REPMRABYPS    BEGSR
009300120627     C*
009400120627     C* Reperimento tabella MRABYPS
009500120627     C                   CLEAR                   InDMRABYPS
009600120627     C                   MOVE(P)   'MRA'         tbeCOD
009700120627     C                   MOVEL(p)  'TRBMBYPSR'   tbeKE1
009800120628     C                   MOVEL(p)  'AV1'         tbeKE2
009900120627     C     KEYTBE01P     CHAIN     TNTBE01L
010000120627     C                   IF        %found(TNTBE01L) AND
010100120627     C                             tbeATB <> 'A'
010200120627     C                   SETOFF                                       60
010300120627     C                   MOVEL     tbeUNI        InDMRABYPS
010400120627     C                   ELSE
010500120627     C                   SETON                                        60
010600120628     C                   EVAL      wrkMsgCED = 'Record AV1 non trovato in'+
010700120627     C                                      ' tabella MRABYPS, avvisare il CED.'
010800120627     C                   ENDIF
010900120627     C*
011000120627     C                   ENDSR
011100010912     C*------------------------------------------------------------------------*
011200010912     C* SNDEML - COMPONE TESTO CORPO MESSAGGI OED INVIA E-MAIL
011300120627     C* Imposto campo contenente la stringa per driver TRBMBYPSR
011400010912     C*------------------------------------------------------------------------*
011500010912     C     SNDEML        BEGSR
011600060807     C*
011700060807     C                   movel     *blanks       wrkMsgCED
011800010912     C*
011900010918     C*  Mi posiziono all'inizio del file tiabl04l
012000010918     C     *loval        SETLL     tiabl04l
012100010912     C*  E scorro tutto il file
012200010918     C                   READ      tiabl04l
012300010918     C                   DOW       not %eof(tiabl04l)
012400010912     C*
012500010918     C* Se record è da inviare
012600010918     C                   IF        ablINV = '0'
012700060807     C*
012800060807     C* Compogo il messaggio da inviare a ced con il resoconto d quanto spedito
012900060809     C                   EVAL      wrkMsgCED = %trim(wrkMsgCED) + 'Cliente: '
013000060822     C                             +ablUTE+'   E-mail: '+%trim(ablEML)+':/N'
013100060811     C*
013200060811     C* Imposto quindi i dati utenti estesi
013300120627     C                   clear                   intrtcm1ds
013400120627     C                   EVAL      §CM1MITT = %trim(x_§MRAMIT1L)
013500060811     C                   EVAL      §CM1VAR  = '*OBJM*' + 'Abilitazione ai ' +
013600060824     C                                        'servizi ON LINE ' + %trim(ablUTE)
013700120627     C                   EVAL      §CM1TIPS = x_§MRAREG
013800120627     C                   EVAL      §CM1IDP  = x_§MRAIDPRO
013900060811     C                   MOVEL(P)  '046'         §CM1PO
014000060811     C                   EVAL      §CM1DST  = ablEML
014100120627     C                   clear                   trul33ds
014200120627     C                   EVAL      i33Cnu = 24
014300120627     C                   EVAL      i33num = 1
014400120627     C                   EVAL      kpjbu = trul33ds
014500120627     C                   CALLP     trul33r(kpjba)
014600120627     C                   EVAL      trul33ds = kpjbu
014700120627     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
014800120627     C                   EVAL      §CM1ATT='01'
014900120627     C                   EVAL      §CM1TOTATT='01'
015000060811     C*
015100120627     C                   EVAL      inTags='§§Tag01§§=='+%trim(ablRSP)+'||'+
015200120627     C                                    '§§Tag02§§=='+%trim(ablUTE)+'||'+
015300120627     C                                    '§§Tag03§§=='+%trim(ablPWD)+'||'
015400120627     C*
015500120627     C                   EVAL      Esito = 0
015600120627     C*
015700120627     C                   CALLP     Bartmail_insert
015800120627     C                             (inTags:indmrabyps:intrtcm1ds:
015900120627     C                              %len(inTags):Esito)
016000060811     C*
016100120627     C                   IF        Esito = 0
016200120627     C*
016300010919     C                   EVAL      contaMOD = contaMOD + 1
016400010914     C*
016500010914     C* Dopo l'invio fleggo il record come "già inviato"
016600010919     C                   EVAL      ablINV = '1'
016700010919     C                   UPDATE    tiabl000
016800010912     C*
016900010912     C                   ENDIF
017000060811     C                   ENDIF
017100060811     C*
017200010912     C* Proseguo con la lettura
017300010918     C                   READ      tiabl04l
017400010912     C*
017500010912     C                   ENDDO
017600010912     C*
017700060811     C                   ENDSR
017800120627     C*-------------------------------------------------------------*
017900120627     C* controllo stringa                                           *
018000120627     C*-------------------------------------------------------------*
018100120627     C     sr_chkstr     Begsr
018200120627     C                   EVAL      string=%xlate('||':'  ':string)
018300120627     C                   EVAL      string=%xlate('==':'  ':string)
018400120627     C                   endsr
018500010912     C*------------------------------------------------------------------------*
018600010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
018700010912     C*------------------------------------------------------------------------*
018800010912     C     *INZSR        BEGSR
018900010912     C*---
019000010912     C* RICEVIMENTO PARAMETRI
019100010912     C*---
019200010912     C     *ENTRY        PLIST
019300010912     C                   PARM                    KPJBA
019400060811     C*
019500060811     C* CHIAVI DI LETTURA
019600060811     C* TNTBE01L
019700060811     C     KEYTBE01P     KLIST
019800060811     C                   KFLD                    tbeCOD
019900060811     C                   KFLD                    tbeKE1
020000060811     C                   KFLD                    tbeKE2
020100010912     C*
020200010912     C                   ENDSR
