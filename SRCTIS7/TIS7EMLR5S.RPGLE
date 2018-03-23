000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120
000500100920     FAZORG01L  IF   E           K DISK
000600100920     FTABEL00F  IF   E           K DISK
000700060811     FTNTBE01L  IF   E           K DISK
000800060811     FPRTEMAIL  O    F  198        PRINTER OFLIND(*INOF) USROPN
000900010912
001000060811     D*------------------
001100100920     D* DS ESTERNE
001200060811     D*------------------
001300060811     D DMRA          E DS
001400100920     D DS3C          E DS
001500100920     D D3CP          E DS
001600100920     D OG140         E DS
001700060811     D*------------------
001800060811     D* DS ridefinizione dati utente estesi spool x mailing
001900060811     D*------------------
002000060811     D TRTCM1DS      E DS
002100060811     D*------------------
002200060811     D* VARIABILI DI WRK
002300060811     D*------------------
002400080526     D wrkEml          S            253    inz(*blanks)
002500080526     D wrkOgg          S             44    inz(*blanks)
002600080526     D wrkMsgCED       S           5000    inz(*blanks)
002700080526     D OTXT1           S            100    inz(*blanks)
002800080526     D OTXT2           S            100    inz(*blanks)
002900100920     D OTXT3           S            100    inz(*blanks)
003000100920     D OTXT4           S            100    inz(*blanks)
003100100920     D OTXT5           S            100    inz(*blanks)
003200100920     D In_Ksc7         S              7    inz(*blanks)
003300100920     D wNRS            S              2    inz(*blanks)
003400100920     D wNCD            S              7    inz(*blanks)
003500100920     D wNCA            S              7    inz(*blanks)
003600100920     D wPOC            S              3    inz(*blanks)
003700010919     D*------------
003800010912
003900010912
004000000414     C*------------------------------------------------------------------------*
004100000414     C* MAIN LINE
004200000414     C*------------------------------------------------------------------------*
004300100920     C*
004400100920     C                   SETOFF                                       60
004500060811     C*
004600060811     C* Reperisco dati tabellati (Tabella 'MRA')
004700060811     C                   EXSR      REPMRA
004800060811     C*
004900060811     C* Se tutto ok proseguo con l'effettiva elaborazione
005000060811     C                   IF        *IN60 = *OFF
005100010919     C                   EXSR      SNDEML
005200080526     C                   ELSE
005300080526     C* Se errore mando email a ced con indicazione
005400120305     C                   eval      wrkEml = 'ced@brt.it'
005500080526     C                   eval      wrkOgg = 'VAS - Errore: ' +
005600100920     C                                      'conferma attivazione EasySped'
005700080526     C                   CALL(e)   'TIS701C'
005800080526     C                   PARM                    wrkEml
005900080526     C                   PARM                    wrkOgg
006000080526     C                   PARM                    wrkMsgCED
006100060811     C                   ENDIF
006200010912     C*
006300010912     C                   SETON                                        LR
006400060811     C*------------------------------------------------------------------------*
006500060811     C     REPMRA        BEGSR
006600060811     C*
006700060811     C* Reperimento tabella MRA
006800060811     C                   CLEAR                   DMRA
006900060811     C                   MOVE(P)   'MRA'         tbeCOD
007000100920     C                   MOVEL(P)  'TIS7EMLR5'   tbeKE1
007100060811     C                   MOVE      *blanks       tbeKE2
007200100920     C     KEYTBE01P_1P  CHAIN     TNTBE01L
007300060811     C                   IF        %found(TNTBE01L) AND
007400060811     C                             tbeATB <> 'A'
007500060811     C                   MOVEL     tbeUNI        DMRA
007600060811     C                   ELSE
007700060811     C                   SETON                                        60
007800100920     C                   EVAL      wrkMsgCED = 'Record TIS7EMLR5 non trovato '+
007900080526     C                                      'in tabella MRA, avvisare il CED.'
008000060811     C                   ENDIF
008100060811     C*
008200060811     C                   ENDSR
008300010912     C*------------------------------------------------------------------------*
008400080526     C* SNDEML - COMPONE TESTO CORPO MESSAGGI ED INVIA E-MAIL
008500010912     C*------------------------------------------------------------------------*
008600010912     C     SNDEML        BEGSR
008700100920     C*
008800100920     C* Reperisco informazioni "tabellari" sul cliente
008900100920     C                   EXSR      RTVTAB
009000100920     C*
009100100920     C* Se no errore procedo
009200100920     C                   IF        not *IN60
009300060811     C*
009400060811     C* Imposto quindi i dati utenti estesi
009500060811     C                   EVAL      §CM1MITT = %trim(§MRAMIT1L)
009600060811     C                   EVAL      §CM1TIPS = §MRAREG
009700060811     C                   EVAL      §CM1IDP  = §MRAIDPRO
009800100920 xxx C                   MOVEL(P)  '046'         §CM1PO
009900110524     C                   EVAL      §CM1VAR  = '*OBJM*' + 'BRT S.p.A. - '    +
010000100920     C                                      'Conferma attivazione EasySped - ' +
010100080603     C                                        '(Cli: '+ %trim(In_Ksc7) + ')'
010200120305     C                   EVAL      §CM1DST  = 'ced'+wPOC+'@brt.it ; ' +
010300120305     C                                        'poc'+wPOC+'@brt.it ; ' +
010400120305     C                                        'ced@brt.it'
010500060811     C*
010600060811     C* Eseguo override x generare uno spool ad "hoc"
010700060811     C                   CALL      'TRUL83C1'
010800060811     C                   PARM      'P'           OPZIONE           1
010900060811     C                   PARM      'PRTEMAIL'    PRTFILE          10
011000060811     C                   PARM                    TRTCM1DS
011100060811     C                   PARM                    §MRAOUTQI
011200060811     C                   PARM                    Esito             1
011300060811     C*
011400060811     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
011500060811     C                   IF        Esito = '2'
011600100920     C                   SETON                                        60
011700100920     C                   EVAL      wrkMsgCED = 'Errore in OVRPRTF e mail '+
011800100920     C                                      'conferma attivazione non inviata.'
011900060811     C                   ELSE
012000060811     C*
012100060811     C* Eseguo la routine d generazione dello spool
012200100920     C  N60              EXSR      CRTSPOOL
012300060811     C*
012400060811     C* Eseguo delete override precedente
012500060811     C                   CALL      'TRUL83C1'
012600060811     C                   PARM      'D'           OPZIONE
012700060811     C                   PARM                    PRTFILE
012800060811     C                   PARM                    TRTCM1DS
012900060811     C                   PARM                    §MRAOUTQI
013000060811     C                   PARM                    Esito
013100010912     C*
013200060811     C                   ENDIF
013300100920     C                   ENDIF
013400010912     C*
013500060811     C                   ENDSR
013600100920     C*------------------------------------------------------------------------*
013700100920     C* REPERIMENTO DATI CLIENTE
013800100920     C*------------------------------------------------------------------------*
013900100920     C     RTVTAB        BEGSR
014000100920     C*
014100100920     C                   CLEAR                   DS3C
014200100920     C                   CLEAR                   D3CP
014300100920     C                   CLEAR                   OG140
014400100920     C*
014500100920     C* Se tutto ok procedo => Reperimento organigramma
014600100920     C                   IF        not *IN60
014700100920     C                   MOVEL(P)  In_Ksc7       wFIL              3 0
014800100920     C     wFIL          CHAIN     AZORG01L
014900100920     C                   IF        %found(AZORG01L)
015000100920     C                   MOVEL(P)  ORGDE0        OG140
015100100920     C                   EVAL      wPOC = §ogapo
015200100920     C                   ELSE
015300100920     C                   SETON                                        60
015400100920     C                   EVAL      wrkMsgCED = 'Filiale cliente ' + In_Ksc7 +
015500100920     C                                 ' non trovata in organigramma.'
015600100920     C                   ENDIF
015700100920     C                   ENDIF
015800100920     C*
015900100920     C* Se tutto ok procedo => Reperimento tabella 3C
016000100920     C                   IF        not *IN60
016100100920     C                   Z-ADD     1             tblKUT
016200100920     C                   MOVE(P)   '3C'          tblCOD
016300100920     C                   MOVEL(P)  In_Ksc7       tblKEY
016400100920     C     KEYTAB        CHAIN     TABEL00F
016500100920     C                   IF        %found(TABEL00F) AND
016600100920     C                             tblFLG = *blanks
016700100920     C                   MOVEL(P)  tblUNI        DS3C
016800100920     C* Elaboro solo se tipo "EasySped"
016900101005     C                   IF        §3CCBA = 'ESYSP'  or  §3Ccba = 'ESVAL'
017000100920     C                   ELSE
017100100920     C                   SETON                                        60
017200100920     C                   EVAL      wrkMsgCED = 'Errore: cliente non di tipo '+
017300100920     C                                 'EasySped'
017400100920     C                   ENDIF
017500100920     C*
017600100920     C                   ELSE
017700100920     C                   SETON                                        60
017800100920     C                   EVAL      wrkMsgCED = 'Errore in tabella 3C/3CP e '+
017900100920     C                                 'mail conferma attivazione non inviata.'
018000100920     C                   ENDIF
018100100920     C                   ENDIF
018200100920     C*
018300100920     C* Se tutto ok procedo => Reperimento tabella 3CP
018400100920     C                   IF        not *IN60
018500100920     C                   MOVE(P)   '3CP'         tbeCOD
018600100920     C                   MOVEL(P)  In_Ksc7       tbeKE1
018700100920     C     KEYTBE01P_2P  CHAIN     TNTBE01L
018800100920     C                   IF        %found(TNTBE01L) AND
018900100920     C                             tbeATB <> 'A'
019000100920     C                   MOVEL     tbeUNI        D3CP
019100100920     C                   EVAL      wNRS = %subst(tbeKE2:1:2)
019200100920     C                   EVAL      wNCD = %subst(tbeKE2:3:7)
019300100920     C                   MOVEL(P)  §3CPAL        wNCA
019400100920     C                   ELSE
019500100920     C                   SETON                                        60
019600100920     C                   EVAL      wrkMsgCED = 'Errore in tabella 3C/3CP e '+
019700100920     C                                 'mail conferma attivazione non inviata.'
019800100920     C                   ENDIF
019900100920     C                   ENDIF
020000100920     C*
020100100920     C                   ENDSR
020200060811     C*------------------------------------------------------------------------*
020300060811     C* CREAZIONE DELLO SPOOL CORRENTE
020400060811     C*------------------------------------------------------------------------*
020500060811     C     CRTSPOOL      BEGSR
020600060811     C*
020700060811     C* Compongo le righe dello spool
020800100921     C                   EVAL      OTXT2 ='§§Tag01§§#'+%trim(§3crag)+
020900080603     C                                     '}{'
021000100921     C                   EVAL      OTXT1 ='§§Tag02§§#'+%trim(In_Ksc7)+
021100080526     C                                     '}{'
021200100920     C                   EVAL      OTXT3 ='§§Tag03§§#'+%trim(wNRS)+
021300100920     C                                     '}{'
021400100920     C                   EVAL      OTXT4 ='§§Tag04§§#'+%trim(wNCD)+
021500100920     C                                     '}{'
021600100920     C                   EVAL      OTXT5 ='§§Tag05§§#'+%trim(wNCA)+
021700100920     C                                     '}{'
021800060811     C*
021900060811     C* Creo lo spool
022000060811     C                   OPEN      PRTEMAIL
022100060811     C                   EXCEPT    DET
022200060811     C                   CLOSE     PRTEMAIL
022300060811     C*
022400060811     C                   ENDSR
022500010912     C*------------------------------------------------------------------------*
022600010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
022700010912     C*------------------------------------------------------------------------*
022800010912     C     *INZSR        BEGSR
022900010912     C*---
023000010912     C* RICEVIMENTO PARAMETRI
023100010912     C*---
023200010912     C     *ENTRY        PLIST
023300080526     C                   PARM                    In_Ksc            8
023400080526     C*
023500080526     C                   EVAL      In_Ksc7 = %subst(In_Ksc:2:7)
023600060811     C*
023700060811     C* CHIAVI DI LETTURA
023800100920     C*
023900100920     C* TABEL00F - Completa
024000100920     C     KEYTAB        KLIST
024100100920     C                   KFLD                    tblKUT
024200100920     C                   KFLD                    tblCOD
024300100920     C                   KFLD                    tblKEY
024400100920     C*
024500100920     C* TNTBE01L - Parziale
024600100920     C     KEYTBE01P_1P  KLIST
024700060811     C                   KFLD                    tbeCOD
024800060811     C                   KFLD                    tbeKE1
024900060811     C                   KFLD                    tbeKE2
025000100920     C*
025100100920     C* TNTBE01L - Parziale
025200100920     C     KEYTBE01P_2P  KLIST
025300100920     C                   KFLD                    tbeCOD
025400100920     C                   KFLD                    tbeKE1
025500010912     C*
025600010912     C                   ENDSR
025700060811     O*------------------------------------------------------------------------*
025800060811     O* STAMPA
025900060811     O*------------------------------------------------------------------------*
026000060811     OPRTEMAIL  E            DET         1
026100080603     O                       OTXT1               +1
026200060811     O          E            DET         1
026300080603     O                       OTXT2               +1
026400100920     O          E            DET         1
026500100920     O                       OTXT3               +1
026600100920     O          E            DET         1
026700100920     O                       OTXT4               +1
026800100920     O          E            DET         1
026900100920     O                       OTXT5               +1
