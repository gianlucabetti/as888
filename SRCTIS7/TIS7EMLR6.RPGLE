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
002400110705     D OTXT3           S            100    inz(*blanks)
002500110705     D OTXT4           S            100    inz(*blanks)
002600110705     D OTXT5           S            100    inz(*blanks)
002700110705     D OTXT6           S            100    inz(*blanks)
002800010919     D*------------
002900010912
003000010912
003100000414     C*------------------------------------------------------------------------*
003200000414     C* MAIN LINE
003300000414     C*------------------------------------------------------------------------*
003400060811     C*
003500060811     C* Reperisco dati tabellati (Tabella 'MRA')
003600060811     C                   EXSR      REPMRA
003700060811     C*
003800060811     C* Se tutto ok proseguo con l'effettiva elaborazione
003900060811     C                   IF        *IN60 = *OFF
004000010919     C                   EXSR      SNDEML
004100080526     C                   ELSE
004200080526     C* Se errore mando email a ced con indicazione
004300120305     C                   eval      wrkEml = 'ced@brt.it'
004400080526     C                   eval      wrkOgg = 'VAS - Errore: ' +
004500080526     C                                      'conferma ricezione file BARTVAS'
004600080526     C                   CALL(e)   'TIS701C'
004700080526     C                   PARM                    wrkEml
004800080526     C                   PARM                    wrkOgg
004900080526     C                   PARM                    wrkMsgCED
005000060811     C                   ENDIF
005100010912     C*
005200010912     C                   SETON                                        LR
005300060811     C*------------------------------------------------------------------------*
005400060811     C     REPMRA        BEGSR
005500060811     C*
005600060811     C* Reperimento tabella MRA
005700060811     C                   CLEAR                   DMRA
005800060811     C                   MOVE(P)   'MRA'         tbeCOD
005900110705     C                   MOVEL(p)  'TIS7EMLR6'   tbeKE1
006000060811     C                   MOVE      *blanks       tbeKE2
006100060811     C     KEYTBE01P     CHAIN     TNTBE01L
006200060811     C                   IF        %found(TNTBE01L) AND
006300060811     C                             tbeATB <> 'A'
006400060811     C                   SETOFF                                       60
006500060811     C                   MOVEL     tbeUNI        DMRA
006600060811     C                   ELSE
006700060811     C                   SETON                                        60
006800110705     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR6 non trovato '+
006900080526     C                                      'in tabella MRA, avvisare il CED.'
007000060811     C                   ENDIF
007100060811     C*
007200060811     C                   ENDSR
007300010912     C*------------------------------------------------------------------------*
007400080526     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
007500010912     C*------------------------------------------------------------------------*
007600010912     C     SNDEML        BEGSR
007700060811     C*
007800060811     C* Imposto quindi i dati utenti estesi
007900060811     C                   EVAL      §CM1MITT = %trim(§MRAMIT1L)
008000060811     C                   EVAL      §CM1TIPS = §MRAREG
008100060811     C                   EVAL      §CM1IDP  = §MRAIDPRO
008200060811     C                   MOVEL(P)  '046'         §CM1PO
008300120305     C                   EVAL      §CM1VAR  = '*OBJM*' + ' BRT - ' +
008400110705     C                             'Segnalazione anomalia: bolla senza ID'
008500171006     C                   EVAL      §CM1DST  = 'cedlog@brt.it'
008600060811     C*
008700060811     C* Eseguo override x generare uno spool ad "hoc"
008800060811     C                   CALL      'TRUL83C1'
008900060811     C                   PARM      'P'           OPZIONE           1
009000060811     C                   PARM      'PRTEMAIL'    PRTFILE          10
009100060811     C                   PARM                    TRTCM1DS
009200060811     C                   PARM                    §MRAOUTQI
009300060811     C                   PARM                    Esito             1
009400060811     C*
009500060811     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
009600060811     C                   IF        Esito = '2'
009700060811     C                   ELSE
009800060811     C*
009900060811     C* Eseguo la routine d generazione dello spool
010000060811     C                   EXSR      CRTSPOOL
010100060811     C*
010200060811     C* Eseguo delete override precedente
010300060811     C                   CALL      'TRUL83C1'
010400060811     C                   PARM      'D'           OPZIONE
010500060811     C                   PARM                    PRTFILE
010600060811     C                   PARM                    TRTCM1DS
010700060811     C                   PARM                    §MRAOUTQI
010800060811     C                   PARM                    Esito
010900010912     C*
011000060811     C                   ENDIF
011100010912     C*
011200060811     C                   ENDSR
011300060811     C*------------------------------------------------------------------------*
011400060811     C* CREAZIONE DELLO SPOOL CORRENTE
011500060811     C*------------------------------------------------------------------------*
011600060811     C     CRTSPOOL      BEGSR
011700060811     C*
011800060811     C* Compongo le righe dello spool
011900110705     C                   EVAL      OTXT1 ='§§Tag01§§#'+%trim(VABRSD)+
012000080603     C                                     '}{'
012100110705     C                   EVAL      OTXT2 ='§§Tag02§§#'+%trim(VABLOD)+
012200110705     C                                     '}{'
012300110705     C                   EVAL      OTXT3 ='§§Tag03§§#'+
012400110705     C                                     %trim(%editc(VABDSP:'X'))+
012500110705     C                                     '}{'
012600110705     C                   EVAL      OTXT4 ='§§Tag04§§#'+
012700110705     C                                     %trim(%editc(VABCCM:'X'))+
012800110705     C                                     '}{'
012900110705     C                   EVAL      OTXT5 ='§§Tag05§§#'+%trim(ACORAG)+
013000110705     C                                     '}{'
013100110705     C                   EVAL      OTXT6 ='§§Tag06§§#'+%trim(BORDERO)+
013200110705     C                                     '}{'
013300060811     C*
013400060811     C* Creo lo spool
013500060811     C                   OPEN      PRTEMAIL
013600060811     C                   EXCEPT    DET
013700060811     C                   CLOSE     PRTEMAIL
013800060811     C*
013900060811     C                   ENDSR
014000010912     C*------------------------------------------------------------------------*
014100010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
014200010912     C*------------------------------------------------------------------------*
014300010912     C     *INZSR        BEGSR
014400010912     C*---
014500010912     C* RICEVIMENTO PARAMETRI
014600010912     C*---
014700010912     C     *ENTRY        PLIST
014800110705     C                   PARM                    VABRSD           35
014900110705     C                   PARM                    VABLOD           35
015000110705     C                   PARM                    VABDSP            8 0
015100110705     C                   PARM                    VABCCM            7 0
015200110705     C                   PARM                    ACORAG           48
015300110705     C                   PARM                    BORDERO          35
015400060811     C*
015500060811     C* CHIAVI DI LETTURA
015600060811     C* TNTBE01L
015700060811     C     KEYTBE01P     KLIST
015800060811     C                   KFLD                    tbeCOD
015900060811     C                   KFLD                    tbeKE1
016000060811     C                   KFLD                    tbeKE2
016100010912     C*
016200010912     C                   ENDSR
016300060811     O*------------------------------------------------------------------------*
016400060811     O* STAMPA
016500060811     O*------------------------------------------------------------------------*
016600060811     OPRTEMAIL  E            DET         1
016700080603     O                       OTXT1               +1
016800060811     O          E            DET         1
016900080603     O                       OTXT2               +1
017000110705     O          E            DET         1
017100110705     O                       OTXT3               +1
017200110705     O          E            DET         1
017300110705     O                       OTXT4               +1
017400110705     O          E            DET         1
017500110705     O                       OTXT5               +1
017600110705     O          E            DET         1
017700110705     O                       OTXT6               +1
