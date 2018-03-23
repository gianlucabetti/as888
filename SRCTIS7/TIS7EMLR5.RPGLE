000100000414
000200120627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300120627     h dftactgrp(*no) BNDDIR('TRBM') ACTGRP('BARTVAS')
000400000414
000500001120
000600100920     FAZORG01L  IF   E           K DISK
000700100920     FTABEL00F  IF   E           K DISK
000800060811     FTNTBE01L  IF   E           K DISK
000900010912
001000060811     D*------------------
001100100920     D* DS ESTERNE
001200060811     D*------------------
001300120627     D kpjba         E DS
001400120627     D DMRA          E DS                  prefix(X_)
001500100920     D DS3C          E DS
001600100920     D D3CP          E DS
001700100920     D OG140         E DS
001800060811     D*------------------
001900060811     D* DS ridefinizione dati utente estesi spool x mailing
002000060811     D*------------------
002100120627     D TRTCM1DS      E DS                  prefix(obs_)
002200060811     D*------------------
002300060811     D* VARIABILI DI WRK
002400060811     D*------------------
002500080526     D wrkEml          S            253    inz(*blanks)
002600080526     D wrkOgg          S             44    inz(*blanks)
002700080526     D wrkMsgCED       S           5000    inz(*blanks)
002800100920     D In_Ksc7         S              7    inz(*blanks)
002900100920     D wNRS            S              2    inz(*blanks)
003000100920     D wNCD            S              7    inz(*blanks)
003100100920     D wNCA            S              7    inz(*blanks)
003200100920     D wPOC            S              3    inz(*blanks)
003300120627     D inTags          s          65535A   VARYING
003400120627     d string          s            200a   varying
003500120627     d Esito           s             10I 0
003600010919     D*------------
003700010912
003800120627      // Definizione prototipi procedure.
003900120627
004000120627     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
004100120627     D trul33r         PR
004200120627     D                                     EXTPGM('TRUL33R')
004300120627     D  kpjba
004400120627     D                                     LIKEDS(kpjba)
004500120627     d trul33ds      e ds
004600010912
004700000414     C*------------------------------------------------------------------------*
004800000414     C* MAIN LINE
004900000414     C*------------------------------------------------------------------------*
005000100920     C*
005100100920     C                   SETOFF                                       60
005200060811     C*
005300060811     C* Reperisco dati tabellati (Tabella 'MRA')
005400060811     C                   EXSR      REPMRA
005500120627     C*
005600120627     C* Reperisco dati tabellati (Tabella 'MRABYPS')
005700120627     C                   EXSR      REPMRABYPS
005800060811     C*
005900060811     C* Se tutto ok proseguo con l'effettiva elaborazione
006000060811     C                   IF        *IN60 = *OFF
006100010919     C                   EXSR      SNDEML
006200080526     C                   ELSE
006300080526     C* Se errore mando email a ced con indicazione
006400120305     C                   eval      wrkEml = 'ced@brt.it'
006500080526     C                   eval      wrkOgg = 'VAS - Errore: ' +
006600100920     C                                      'conferma attivazione EasySped'
006700080526     C                   CALL(e)   'TIS701C'
006800080526     C                   PARM                    wrkEml
006900080526     C                   PARM                    wrkOgg
007000080526     C                   PARM                    wrkMsgCED
007100060811     C                   ENDIF
007200010912     C*
007300010912     C                   SETON                                        LR
007400060811     C*------------------------------------------------------------------------*
007500060811     C     REPMRA        BEGSR
007600060811     C*
007700060811     C* Reperimento tabella MRA
007800060811     C                   CLEAR                   DMRA
007900060811     C                   MOVE(P)   'MRA'         tbeCOD
008000100920     C                   MOVEL(P)  'TIS7EMLR5'   tbeKE1
008100060811     C                   MOVE      *blanks       tbeKE2
008200100920     C     KEYTBE01P_1P  CHAIN     TNTBE01L
008300060811     C                   IF        %found(TNTBE01L) AND
008400060811     C                             tbeATB <> 'A'
008500060811     C                   MOVEL     tbeUNI        DMRA
008600060811     C                   ELSE
008700060811     C                   SETON                                        60
008800100920     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR5 non trovato '+
008900080526     C                                      'in tabella MRA, avvisare il CED.'
009000060811     C                   ENDIF
009100060811     C*
009200060811     C                   ENDSR
009300120627     C*------------------------------------------------------------------------*
009400120627     C     REPMRABYPS    BEGSR
009500120627     C*
009600120627     C* Reperimento tabella MRABYPS
009700120627     C                   CLEAR                   InDMRABYPS
009800120627     C                   MOVE(P)   'MRA'         tbeCOD
009900120627     C                   MOVEL(p)  'TRBMBYPSR'   tbeKE1
010000120628     C                   MOVEL(p)  'ESY'         tbeKE2
010100120627     C     KEYTBE01P_1P  CHAIN     TNTBE01L
010200120627     C                   IF        %found(TNTBE01L) AND
010300120627     C                             tbeATB <> 'A'
010400120627     C                   SETOFF                                       60
010500120627     C                   MOVEL     tbeUNI        InDMRABYPS
010600120627     C                   ELSE
010700120627     C                   SETON                                        60
010800120628     C                   EVAL      wrkMsgCED = 'Record ESY non trovato in'+
010900120627     C                                      ' tabella MRABYPS, avvisare il CED.'
011000120627     C                   ENDIF
011100120627     C*
011200120627     C                   ENDSR
011300010912     C*------------------------------------------------------------------------*
011400080526     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
011500120627     C* Imposto campo contenente la stringa per driver TRBMBYPSR
011600010912     C*------------------------------------------------------------------------*
011700010912     C     SNDEML        BEGSR
011800100920     C*
011900100920     C* Reperisco informazioni "tabellari" sul cliente
012000100920     C                   EXSR      RTVTAB
012100100920     C*
012200100920     C* Se no errore procedo
012300100920     C                   IF        not *IN60
012400060811     C*
012500060811     C* Imposto quindi i dati utenti estesi
012600120627     C                   EVAL      §CM1MITT = %trim(x_§MRAMIT1L)
012700120627     C                   EVAL      §CM1TIPS = x_§MRAREG
012800120627     C                   EVAL      §CM1IDP  = x_§MRAIDPRO
012900100920 xxx C                   MOVEL(P)  '046'         §CM1PO
013000110524     C                   EVAL      §CM1VAR  = '*OBJM*' + 'BRT S.p.A. - '    +
013100100920     C                                      'Conferma attivazione EasySped - ' +
013200080603     C                                        '(Cli: '+ %trim(In_Ksc7) + ')'
013300120305     C                   EVAL      §CM1DST  = 'ced'+wPOC+'@brt.it ; ' +
013400120305     C                                        'poc'+wPOC+'@brt.it ; ' +
013500120305     C                                        'ced@brt.it'
013600120627     C                   clear                   trul33ds
013700120627     C                   EVAL      i33Cnu = 24
013800120627     C                   EVAL      i33num = 1
013900120627     C                   EVAL      kpjbu = trul33ds
014000120627     C                   CALLP     trul33r(kpjba)
014100120627     C                   EVAL      trul33ds = kpjbu
014200120627     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
014300120627     C                   EVAL      §CM1ATT='01'
014400120627     C                   EVAL      §CM1TOTATT='01'
014500120627     C*
014600120627     C                   EVAL      inTags='§§Tag01§§=='+%trim(§3crag)+
014700120627     C                                     '||'+
014800120627     C                                    '§§Tag02§§=='+%trim(In_Ksc7)+
014900120627     C                                     '||'+
015000120627     C                                    '§§Tag03§§=='+%trim(wNRS)+
015100120627     C                                     '||'+
015200120627     C                                    '§§Tag04§§=='+%trim(wNCD)+
015300120627     C                                     '||'+
015400120627     C                                    '§§Tag05§§=='+%trim(wNCA)+
015500120627     C                                     '||'
015600120627     C*
015700120627     C                   EVAL      Esito = 0
015800120627     C*
015900120627     C                   CALLP     Bartmail_insert
016000120627     C                             (inTags:indmrabyps:intrtcm1ds:
016100120627     C                              %len(inTags):Esito)
016200010912     C*
016300100920     C                   ENDIF
016400010912     C*
016500060811     C                   ENDSR
016600100920     C*------------------------------------------------------------------------*
016700100920     C* REPERIMENTO DATI CLIENTE
016800100920     C*------------------------------------------------------------------------*
016900100920     C     RTVTAB        BEGSR
017000100920     C*
017100100920     C                   CLEAR                   DS3C
017200100920     C                   CLEAR                   D3CP
017300100920     C                   CLEAR                   OG140
017400100920     C*
017500100920     C* Se tutto ok procedo => Reperimento organigramma
017600100920     C                   IF        not *IN60
017700100920     C                   MOVEL(P)  In_Ksc7       wFIL              3 0
017800100920     C     wFIL          CHAIN     AZORG01L
017900100920     C                   IF        %found(AZORG01L)
018000100920     C                   MOVEL(P)  ORGDE0        OG140
018100100920     C                   EVAL      wPOC = §ogapo
018200100920     C                   ELSE
018300100920     C                   SETON                                        60
018400100920     C                   EVAL      wrkMsgCED = 'Filiale cliente ' + In_Ksc7 +
018500100920     C                                 ' non trovata in organigramma.'
018600100920     C                   ENDIF
018700100920     C                   ENDIF
018800100920     C*
018900100920     C* Se tutto ok procedo => Reperimento tabella 3C
019000100920     C                   IF        not *IN60
019100100920     C                   Z-ADD     1             tblKUT
019200100920     C                   MOVE(P)   '3C'          tblCOD
019300100920     C                   MOVEL(P)  In_Ksc7       tblKEY
019400100920     C     KEYTAB        CHAIN     TABEL00F
019500100920     C                   IF        %found(TABEL00F) AND
019600100920     C                             tblFLG = *blanks
019700100920     C                   MOVEL(P)  tblUNI        DS3C
019800100920     C* Elaboro solo se tipo "EasySped"
019900101005     C                   IF        §3CCBA = 'ESYSP'  or  §3Ccba = 'ESVAL'
020000100920     C                   ELSE
020100100920     C                   SETON                                        60
020200100920     C                   EVAL      wrkMsgCED = 'Errore: cliente non di tipo '+
020300100920     C                                 'EasySped'
020400100920     C                   ENDIF
020500100920     C*
020600100920     C                   ELSE
020700100920     C                   SETON                                        60
020800100920     C                   EVAL      wrkMsgCED = 'Errore in tabella 3C/3CP e '+
020900100920     C                                 'mail conferma attivazione non inviata.'
021000100920     C                   ENDIF
021100100920     C                   ENDIF
021200100920     C*
021300100920     C* Se tutto ok procedo => Reperimento tabella 3CP
021400100920     C                   IF        not *IN60
021500100920     C                   MOVE(P)   '3CP'         tbeCOD
021600100920     C                   MOVEL(P)  In_Ksc7       tbeKE1
021700100920     C     KEYTBE01P_2P  CHAIN     TNTBE01L
021800100920     C                   IF        %found(TNTBE01L) AND
021900100920     C                             tbeATB <> 'A'
022000100920     C                   MOVEL     tbeUNI        D3CP
022100100920     C                   EVAL      wNRS = %subst(tbeKE2:1:2)
022200100920     C                   EVAL      wNCD = %subst(tbeKE2:3:7)
022300100920     C                   MOVEL(P)  §3CPAL        wNCA
022400100920     C                   ELSE
022500100920     C                   SETON                                        60
022600100920     C                   EVAL      wrkMsgCED = 'Errore in tabella 3C/3CP e '+
022700100920     C                                 'mail conferma attivazione non inviata.'
022800100920     C                   ENDIF
022900100920     C                   ENDIF
023000100920     C*
023100100920     C                   ENDSR
023200010912     C*------------------------------------------------------------------------*
023300010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
023400010912     C*------------------------------------------------------------------------*
023500010912     C     *INZSR        BEGSR
023600010912     C*---
023700010912     C* RICEVIMENTO PARAMETRI
023800010912     C*---
023900010912     C     *ENTRY        PLIST
024000080526     C                   PARM                    In_Ksc            8
024100080526     C*
024200080526     C                   EVAL      In_Ksc7 = %subst(In_Ksc:2:7)
024300060811     C*
024400060811     C* CHIAVI DI LETTURA
024500100920     C*
024600100920     C* TABEL00F - Completa
024700100920     C     KEYTAB        KLIST
024800100920     C                   KFLD                    tblKUT
024900100920     C                   KFLD                    tblCOD
025000100920     C                   KFLD                    tblKEY
025100100920     C*
025200100920     C* TNTBE01L - Parziale
025300100920     C     KEYTBE01P_1P  KLIST
025400060811     C                   KFLD                    tbeCOD
025500060811     C                   KFLD                    tbeKE1
025600060811     C                   KFLD                    tbeKE2
025700100920     C*
025800100920     C* TNTBE01L - Parziale
025900100920     C     KEYTBE01P_2P  KLIST
026000100920     C                   KFLD                    tbeCOD
026100100920     C                   KFLD                    tbeKE1
026200010912     C*
026300010912     C                   ENDSR
