000100000414
000200120627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300120627     h dftactgrp(*no) BNDDIR('TRBM') ACTGRP('BARTVAS')
000400000414
000500001120
000600060811     FTNTBE01L  IF   E           K DISK
000700010912
000800120627     D*------------------
000900120627     D kpjba         E DS
001000060811     D*------------------
001100060811     D* TABELLEA 'MRA'
001200060811     D*------------------
001300120627     D DMRA          E DS                  prefix(X_)
001400060811     D*------------------
001500060811     D* DS ridefinizione dati utente estesi spool x mailing
001600060811     D*------------------
001700120627     D TRTCM1DS      E DS                  prefix(obs_)
001800060811     D*------------------
001900060811     D* VARIABILI DI WRK
002000060811     D*------------------
002100080526     D wrkEml          S            253    inz(*blanks)
002200080526     D wrkOgg          S             44    inz(*blanks)
002300080526     D wrkMsgCED       S           5000    inz(*blanks)
002400080526     D In_Ksc7         S              7    inz(*blanks)
002500120627     D inTags          s          65535A   VARYING
002600120627     d string          s            200a   varying
002700120627     d Esito           s             10I 0
002800120627
002900010919     D*------------
003000010912
003100120627      // Definizione prototipi procedure.
003200120627
003300120627     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
003400120627     D trul33r         PR
003500120627     D                                     EXTPGM('TRUL33R')
003600120627     D  kpjba
003700120627     D                                     LIKEDS(kpjba)
003800120627     d trul33ds      e ds
003900010912
004000000414     C*------------------------------------------------------------------------*
004100000414     C* MAIN LINE
004200000414     C*------------------------------------------------------------------------*
004300120627     C*
004400120627     C* Reperisco dati tabellati (Tabella 'MRA')
004500120627     C                   EXSR      REPMRA
004600120627     C*
004700120627     C* Reperisco dati tabellati (Tabella 'MRABYPS')
004800120627     C                   EXSR      REPMRABYPS
004900060811     C*
005000060811     C* Se tutto ok proseguo con l'effettiva elaborazione
005100060811     C                   IF        *IN60 = *OFF
005200010919     C                   EXSR      SNDEML
005300080526     C                   ELSE
005400080526     C* Se errore mando email a ced con indicazione
005500120305     C                   eval      wrkEml = 'ced@brt.it'
005600080526     C                   eval      wrkOgg = 'VAS - Errore: ' +
005700080526     C                                      'conferma ricezione file BARTVAS'
005800080526     C                   CALL(e)   'TIS701C'
005900080526     C                   PARM                    wrkEml
006000080526     C                   PARM                    wrkOgg
006100080526     C                   PARM                    wrkMsgCED
006200060811     C                   ENDIF
006300010912     C*
006400010912     C                   SETON                                        LR
006500120627     C*------------------------------------------------------------------------*
006600120627     C     REPMRA        BEGSR
006700120627     C*
006800120627     C* Reperimento tabella MRA
006900120627     C                   CLEAR                   DMRA
007000120627     C                   MOVE(P)   'MRA'         tbeCOD
007100120627     C                   MOVEL(p)  'TIS7EMLR4'   tbeKE1
007200120627     C                   MOVE      *blanks       tbeKE2
007300120627     C     KEYTBE01P     CHAIN     TNTBE01L
007400120627     C                   IF        %found(TNTBE01L) AND
007500120627     C                             tbeATB <> 'A'
007600120627     C                   SETOFF                                       60
007700120627     C                   MOVEL     tbeUNI        DMRA
007800120627     C                   ELSE
007900120627     C                   SETON                                        60
008000120627     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR4 non trovato '+
008100120627     C                                      'in tabella MRA, avvisare il CED.'
008200120627     C                   ENDIF
008300120627     C*
008400120627     C                   ENDSR
008500120627     C*------------------------------------------------------------------------*
008600120627     C     REPMRABYPS    BEGSR
008700120627     C*
008800120627     C* Reperimento tabella MRABYPS
008900120627     C                   CLEAR                   InDMRABYPS
009000120627     C                   MOVE(P)   'MRA'         tbeCOD
009100120627     C                   MOVEL(p)  'TRBMBYPSR'   tbeKE1
009200120627     C                   MOVEL(p)  'RV1'         tbeKE2
009300120627     C     KEYTBE01P     CHAIN     TNTBE01L
009400120627     C                   IF        %found(TNTBE01L) AND
009500120627     C                             tbeATB <> 'A'
009600120627     C                   SETOFF                                       60
009700120627     C                   MOVEL     tbeUNI        InDMRABYPS
009800120627     C                   ELSE
009900120627     C                   SETON                                        60
010000120627     C                   EVAL      wrkMsgCED = 'Record RV1 non trovato in'+
010100120627     C                                      ' tabella MRABYPS, avvisare il CED.'
010200120627     C                   ENDIF
010300120627     C*
010400120627     C                   ENDSR
010500060811     C*------------------------------------------------------------------------*
010600120627     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
010700120627     C* Imposto campo contenente la stringa per driver TRBMBYPSR
010800060811     C*------------------------------------------------------------------------*
010900120627     C     SndEml        BEGSR
011000120627     C*
011100120627     C* Imposto i dati utenti estesi
011200120627     C                   clear                   intrtcm1ds
011300120627     C                   EVAL      低M1MITT = %trim(x_冶RAMIT1L)
011400120627     C                   EVAL      低M1TIPS = x_冶RAREG
011500120627     C                   EVAL      低M1IDP  = x_冶RAIDPRO
011600120627     C                   MOVEL(P)  '046'         低M1PO
011700120627     C                   EVAL      低M1VAR  = '*OBJM*' + 'BRT S.p.A. - ' +
011800120627     C                                        'Conferma ricezione file - '  +
011900120627     C                                        '(Cli: '+ %trim(In_Ksc7) + ')'
012000120627     C                   EVAL      低M1DST  = In_EmlDest
012100120627     C                   clear                   trul33ds
012200120627     C                   EVAL      i33Cnu = 24
012300120627     C                   EVAL      i33num = 1
012400120627     C                   EVAL      kpjbu = trul33ds
012500120627     C                   CALLP     trul33r(kpjba)
012600120627     C                   EVAL      trul33ds = kpjbu
012700120627     C                   EVAL      低M1PROG=%subst(%editc(o33nri:'X'):9:7)
012800120627     C                   EVAL      低M1ATT='01'
012900120627     C                   EVAL      低M1TOTATT='01'
013000060811     C*
013100120627     C                   EVAL      inTags='壯Tag01壯=='+%trim(In_Nomefile)+
013200120627     C                                     '||' +
013300120627     C                                    '壯Tag02壯=='+
013400080526     C                                     %trim(%editc(In_TotRcd:'1'))+
013500120627     C                                     '||'
013600060811     C*
013700120627     C                   EVAL      Esito = 0
013800120627     C*
013900120627     C                   CALLP     Bartmail_insert
014000120627     C                             (inTags:indmrabyps:intrtcm1ds:
014100120627     C                              %len(inTags):Esito)
014200060811     C*
014300060811     C                   ENDSR
014400120627     C*-------------------------------------------------------------*
014500120627     C* controllo stringa                                           *
014600120627     C*-------------------------------------------------------------*
014700120627     C     sr_chkstr     Begsr
014800120627     C                   EVAL      string=%xlate('||':'  ':string)
014900120627     C                   EVAL      string=%xlate('==':'  ':string)
015000120627     C                   endsr
015100010912     C*------------------------------------------------------------------------*
015200010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
015300010912     C*------------------------------------------------------------------------*
015400010912     C     *INZSR        BEGSR
015500010912     C*---
015600010912     C* RICEVIMENTO PARAMETRI
015700010912     C*---
015800010912     C     *ENTRY        PLIST
015900080526     C                   PARM                    In_Ksc            8
016000080526     C                   PARM                    In_NomeFile      30
016100080526     C                   PARM                    In_TotRcd         7 0
016200080526     C                   PARM                    In_EmlDest      121
016300080526     C*
016400080526     C                   EVAL      In_Ksc7 = %subst(In_Ksc:2:7)
016500060811     C*
016600060811     C* CHIAVI DI LETTURA
016700060811     C* TNTBE01L
016800060811     C     KEYTBE01P     KLIST
016900060811     C                   KFLD                    tbeCOD
017000060811     C                   KFLD                    tbeKE1
017100060811     C                   KFLD                    tbeKE2
017200010912     C*
017300010912     C                   ENDSR
