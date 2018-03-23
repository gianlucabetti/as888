000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120
000500060811     FTNTBE01L  IF   E           K DISK
000600060811     FPRTEMAIL  O    F  198        PRINTER OFLIND(*INOF) USROPN
000700010912
000800060811     D*------------------
000900060811     D* TABELLEA 'MRA'
001000060811     D*------------------
001100060811     D DMRA          E DS
001200060811     D*------------------
001300060811     D* DS ridefinizione dati utente estesi spool x mailing
001400060811     D*------------------
001500060811     D TRTCM1DS      E DS
001600060811     D*------------------
001700060811     D* VARIABILI DI WRK
001800060811     D*------------------
001900080526     D wrkEml          S            253    inz(*blanks)
002000080526     D wrkOgg          S             44    inz(*blanks)
002100080526     D wrkMsgCED       S           5000    inz(*blanks)
002200080526     D OTXT1           S            100    inz(*blanks)
002300080526     D OTXT2           S            100    inz(*blanks)
002400080526     D In_Ksc7         S              7    inz(*blanks)
002500010919     D*------------
002600010912
002700010912
002800000414     C*------------------------------------------------------------------------*
002900000414     C* MAIN LINE
003000000414     C*------------------------------------------------------------------------*
003100060811     C*
003200060811     C* Reperisco dati tabellati (Tabella 'MRA')
003300060811     C                   EXSR      REPMRA
003400060811     C*
003500060811     C* Se tutto ok proseguo con l'effettiva elaborazione
003600060811     C                   IF        *IN60 = *OFF
003700010919     C                   EXSR      SNDEML
003800080526     C                   ELSE
003900080526     C* Se errore mando email a ced con indicazione
004000120305     C                   eval      wrkEml = 'ced@brt.it'
004100080526     C                   eval      wrkOgg = 'VAS - Errore: ' +
004200080526     C                                      'conferma ricezione file BARTVAS'
004300080526     C                   CALL(e)   'TIS701C'
004400080526     C                   PARM                    wrkEml
004500080526     C                   PARM                    wrkOgg
004600080526     C                   PARM                    wrkMsgCED
004700060811     C                   ENDIF
004800010912     C*
004900010912     C                   SETON                                        LR
005000060811     C*------------------------------------------------------------------------*
005100060811     C     REPMRA        BEGSR
005200060811     C*
005300060811     C* Reperimento tabella MRA
005400060811     C                   CLEAR                   DMRA
005500060811     C                   MOVE(P)   'MRA'         tbeCOD
005600080526     C                   MOVEL(p)  'TIS7EMLR4'   tbeKE1
005700060811     C                   MOVE      *blanks       tbeKE2
005800060811     C     KEYTBE01P     CHAIN     TNTBE01L
005900060811     C                   IF        %found(TNTBE01L) AND
006000060811     C                             tbeATB <> 'A'
006100060811     C                   SETOFF                                       60
006200060811     C                   MOVEL     tbeUNI        DMRA
006300060811     C                   ELSE
006400060811     C                   SETON                                        60
006500080526     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR4 non trovato '+
006600080526     C                                      'in tabella MRA, avvisare il CED.'
006700060811     C                   ENDIF
006800060811     C*
006900060811     C                   ENDSR
007000010912     C*------------------------------------------------------------------------*
007100080526     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
007200010912     C*------------------------------------------------------------------------*
007300010912     C     SNDEML        BEGSR
007400060811     C*
007500060811     C* Imposto quindi i dati utenti estesi
007600060811     C                   EVAL      低M1MITT = %trim(冶RAMIT1L)
007700060811     C                   EVAL      低M1TIPS = 冶RAREG
007800060811     C                   EVAL      低M1IDP  = 冶RAIDPRO
007900060811     C                   MOVEL(P)  '046'         低M1PO
008000110524     C                   EVAL      低M1VAR  = '*OBJM*' + 'BRT S.p.A. - ' +
008100080603     C                                        'Conferma ricezione file - '  +
008200080603     C                                        '(Cli: '+ %trim(In_Ksc7) + ')'
008300080526     C                   EVAL      低M1DST  = In_EmlDest
008400060811     C*
008500060811     C* Eseguo override x generare uno spool ad "hoc"
008600060811     C                   CALL      'TRUL83C1'
008700060811     C                   PARM      'P'           OPZIONE           1
008800060811     C                   PARM      'PRTEMAIL'    PRTFILE          10
008900060811     C                   PARM                    TRTCM1DS
009000060811     C                   PARM                    冶RAOUTQI
009100060811     C                   PARM                    Esito             1
009200060811     C*
009300060811     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
009400060811     C                   IF        Esito = '2'
009500060811     C                   ELSE
009600060811     C*
009700060811     C* Eseguo la routine d generazione dello spool
009800060811     C                   EXSR      CRTSPOOL
009900060811     C*
010000060811     C* Eseguo delete override precedente
010100060811     C                   CALL      'TRUL83C1'
010200060811     C                   PARM      'D'           OPZIONE
010300060811     C                   PARM                    PRTFILE
010400060811     C                   PARM                    TRTCM1DS
010500060811     C                   PARM                    冶RAOUTQI
010600060811     C                   PARM                    Esito
010700010912     C*
010800060811     C                   ENDIF
010900010912     C*
011000060811     C                   ENDSR
011100060811     C*------------------------------------------------------------------------*
011200060811     C* CREAZIONE DELLO SPOOL CORRENTE
011300060811     C*------------------------------------------------------------------------*
011400060811     C     CRTSPOOL      BEGSR
011500060811     C*
011600060811     C* Compongo le righe dello spool
011700080603     C                   EVAL      OTXT1 ='壯Tag01壯#'+%trim(In_Nomefile)+
011800080603     C                                     '}{'
011900080603     C                   EVAL      OTXT2 ='壯Tag02壯#'+
012000080526     C                                     %trim(%editc(In_TotRcd:'1'))+
012100080526     C                                     '}{'
012200060811     C*
012300060811     C* Creo lo spool
012400060811     C                   OPEN      PRTEMAIL
012500060811     C                   EXCEPT    DET
012600060811     C                   CLOSE     PRTEMAIL
012700060811     C*
012800060811     C                   ENDSR
012900010912     C*------------------------------------------------------------------------*
013000010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
013100010912     C*------------------------------------------------------------------------*
013200010912     C     *INZSR        BEGSR
013300010912     C*---
013400010912     C* RICEVIMENTO PARAMETRI
013500010912     C*---
013600010912     C     *ENTRY        PLIST
013700080526     C                   PARM                    In_Ksc            8
013800080526     C                   PARM                    In_NomeFile      30
013900080526     C                   PARM                    In_TotRcd         7 0
014000080526     C                   PARM                    In_EmlDest      121
014100080526     C*
014200080526     C                   EVAL      In_Ksc7 = %subst(In_Ksc:2:7)
014300060811     C*
014400060811     C* CHIAVI DI LETTURA
014500060811     C* TNTBE01L
014600060811     C     KEYTBE01P     KLIST
014700060811     C                   KFLD                    tbeCOD
014800060811     C                   KFLD                    tbeKE1
014900060811     C                   KFLD                    tbeKE2
015000010912     C*
015100010912     C                   ENDSR
015200060811     O*------------------------------------------------------------------------*
015300060811     O* STAMPA
015400060811     O*------------------------------------------------------------------------*
015500060811     OPRTEMAIL  E            DET         1
015600080603     O                       OTXT1               +1
015700060811     O          E            DET         1
015800080603     O                       OTXT2               +1
