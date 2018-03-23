000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060317     Ffiar404l  iF   E           K DISK
000500060317     Ffnarb01l  iF   E           K DISK
000600050909     FTIVGDTMP  UF   E             DISK
000700020508     Ffnvapfdx  O    E             DISK
000800060320     FTIVIR02L  UF   E           K DISK
000900060519     F*TABEL00F  IF   E           K DISK
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400021015     D dataISOda       s               D   datfmt(*ISO)
001500021015     D dataISOa        s               D   datfmt(*ISO)
001600021015     D numMESI         s              5i 0
001700050909     D fnvapds       e ds                  extname(fnvap00t)
001800060519     D*DSQT1         e ds                  inz
001900060320     D prmora          s              4    inz(*blanks)
002000060519     D wPesVDL         s                   like(arbPKC)
002100060519     D wVolVDL         s                   like(arbVLC)
002200020508
002300060320     D*---------------------
002400060320     D* DS "XSRDA8" - CONTROLLA DATA (8)
002500060320     D*---------------------
002600060320     D WLBDA8          DS
002700060320     D  G08DAT                 1      8  0
002800060320     D  G08INV                 9     16  0
002900060320     D  G08ERR                17     17
003000060320     D  G08TGI                18     22  0
003100060320
003200020508     D*------------
003300020508     D* DS di ridefinizione chiave esterna ORM
003400020508     D*------------
003500020508     D                 DS
003600050909     D x_vapPOE                1      3  0
003700050909     D x_vapNSR                4      5  0
003800050909     D x_vapNOR                6     12  0
003900050909     D x_vapNRV               13     14  0
004000020508     D keyORM                  1     14
004100991027
004200060320     C*
004300060320     C* Inizializzo l'esito a 'Y' ovvero => by-passa elaborazione
004400060320     C                   EVAL      wrkesito = 'Y'
004500060320     C*
004600060320     C* Innanzitutto verifico se la traduzione è da eseguire in quanto occorre elaborare 1 volta
004700060320     C* al giorno e solo dopo le ore indicate nei parametri, se nn diversamente indicato dopo
004800060320     C* le ore 09:00
004900060320     C                   EVAL      virKSC = '0' + %subst(prmppt:1:7)
005000060320     C                   EVAL      virTIP = 'VP'
005100060320     C     KEYvir02_P    CHAIN     TIVIR02L
005200060320     C                   IF        %found(TIVIR02L)
005300060320     C*
005400060320     C* Se ora nn espressamente indicata nei parametri in anagrafica imposto le 09:00
005500060320     C                   EVAL      prmora = %subst(prmppt:8:4)
005600060320     C                   IF        prmora = *blanks OR
005700060320     C                             prmora = *zeros
005800060320     C                   EVAL      prmora = '0900'
005900060320     C                   ENDIF
006000060404     C*
006100060404     C* Se data nn espressamente indicata nei parametri in anagrafica imposto la data corrente
006200060404     C                   IF        %subst(virPPT:12:8) = *blanks OR
006300060404     C                             %subst(virPPT:12:8) = *zeros
006400060404     C                   EVAL      %subst(virPPT:12:8) = DATCOR
006500060404     C                   ENDIF
006600060320     C*
006700060320     C* Eseguo quindi la traduzione solo se la data nei parametri in anagrafica nn è
006800060320     C* quella corrente e se l'ora corrente è superiore a quella indicata nei parametri
006900060320     C* in anagrafica.
007000060412     C                   IF        DATCOR <> %subst(virPPT:12:8) AND
007100060412     C                             ORACOR >= prmora
007200060519     C*
007300060519     C* Carico le tabelle occorrenti
007400060519     C*                  CLEAR                   DSQT1
007500060519     C*                  Z-ADD     1             tblKUT
007600060519     C*                  MOVEL     'QT'          tblCOD
007700060519     C*                  MOVEL     '1'           tblKEY
007800060519     C*    KEYtabel      CHAIN     TABEL00F
007900060519     C*                  IF        %found(TABEL00F)
008000060519     C*                  EVAL      DSQT1 = tblUNI
008100060519     C*                  ENDIF
008200060519     C*
008300060519     C* Eseguo traduzione vera e propria
008400060320     C                   EXSR      traduci
008500060320     C*
008600060320     C* Quindi aggiorno i parametri dell'anagrafica
008700060320     C                   EVAL      %subst(virPPT:12:8) = DATCOR
008800060320     C                   UPDATE    TIVIR000
008900060320     C*
009000060320     C* Una volta effettuata la tradzione aggiorno l'esito su OK
009100060320     C                   EVAL      wrkesito = '0'
009200060519     C                   ENDIF
009300060320     C                   ENDIF
009400060320     C*
009500921023     C                   SETON                                        LR
009600991027
009700991027     C     traduci       BEGSR
009800991027     C*
009900050909     C                   READ      TIVGDTMP
010000050909     C                   DOW       not %eof(TIVGDTMP)
010100050909     C                   eval      fnvapds = vgdDTA
010200991027     C*
010300020508     C* Elaboro solo le fasi 500/600/900/999
010400020508     C                   IF        vapFAR = 500 OR
010500020508     C                             vapFAR = 600 OR
010600020508     C                             vapFAR = 900 OR
010700060414     C                             vapFAR = 910 OR
010800020508     C                             vapFAR = 999
010900020508     C*
011000020508     C* Innanzitutto inizializzo il formato record del file tradotto
011100020508     C                   CLEAR                   FNVAPxxx
011200020508     C*
011300060320     C                   IF        %len(%trim(vapRFA)) >= 14
011400060320     C*
011500020508     C                   IF        %subst(%trim(vapRFA):1:1) = 'P'
011600020508     C                   EVAL      fdxMSGCD = 'PUR'
011700020508     C                   ENDIF
011800020508     C                   IF        %subst(%trim(vapRFA):1:1) = 'R'
011900020508     C                   EVAL      fdxMSGCD = 'ROC'
012000020508     C                   ENDIF
012100020508     C*
012200020508     C                   EVAL      fdxREGDT = '20' + %subst(%trim(vapRFA):2:6)
012300020508     C                   EVAL      fdxPKNO  = %subst(%trim(vapRFA):8:7)
012400060320     C*
012500060320     C                   ENDIF
012600020508     C*
012700020508     C                   EVAL      fdxSTNCD = 'BART '
012800020508     C*
012900021004     C* Stabilisco lo status del ritiro
013000021004     C                   IF        vapFAR = 600
013100021004     C                   EVAL      fdxSTSCD = 'RE'
013200021004     C                   ELSE
013300060414     C                   IF        vapFAR = 900
013400060414     C***                          OR (vapFAR = 999 AND
013500060414     C***                              vapCAR = '82')
013600021004     C                   EVAL      fdxSTSCD = 'SB'
013700021004     C                   ELSE
013800060414     C***                IF        vapCAR <> '82'
013900021004     C                   EVAL      fdxSTSCD = vapCAR
014000060519     C***                ENDIF
014100021004     C                   ENDIF
014200021004     C                   ENDIF
014300020508     C*
014400020508     C                   MOVEL     vapDAE        fdxPKDT
014500020508     C                   MOVEL     vapORE        fdxPKTM
014600021015     C*
014700021015     C* Inizializzo il flag di storicizzazione
014800021015     C                   MOVEL     'N'           flgCHK            1
014900021015     C*
015000021015     C* Se trattasi d 1 status x cui occorre reperire l'AWB => reperisco la bolla corrispondente
015100021015     C                   IF        fdxSTSCD = 'RE' OR
015200021015     C                             fdxSTSCD = 'SB'
015300020508     C*
015400020508     C* Reperisco alcuni dati direttamente dalla bolla legata al ritiro
015500060317     C                   EXSR      repBOL
015600021015     C                   ELSE
015700021015     C                   EVAL      flgCHK = 'S'
015800021015     C                   WRITE     FNVAPxxx
015900021015     C                   ENDIF
016000020508     C*
016100021015     C                   ELSE
016200021015     C                   EVAL      flgCHK = 'S'
016300020910     C                   ENDIF
016400021015     C*
016500021015     C* Se comunque la fase ORM è + vecchia d 1 mese => storicizzzo cmq
016600021015     C     *ISO          MOVE      vapDAE        dataISOda
016700021015     C                   TIME                    dataISOa
016800021015     C     dataISOa      SUBDUR    dataISOda     numMESI:*M
016900021015     C                   IF        numMESI >= 1
017000021015     C                   EVAL      flgCHK = 'S'
017100021015     C                   ENDIF
017200020910     C*
017300021015     C                   IF        flgCHK = 'S'
017400050909     C                   DELETE    TIVGD000
017500021015     C                   ENDIF
017600020508     C*
017700050909     C                   READ      TIVGDTMP
017800021015     C                   ENDDO
017900991027     C*
018000910830     C                   ENDSR
018100020508
018200020508
018300020508
018400020508
018500020508      /TITLE Reperimento dati dalle bolle
018600060317     C     REPBOL        BEGSR
018700020508     C*
018800021015     C* Effettuo prima lettura (e implicitamente posizionamento) "fuori ciclo"
018900050909     C                   EVAL      x_vapPOE = vapPOE
019000050909     C                   EVAL      x_vapNSR = vapNSR
019100050909     C                   EVAL      x_vapNOR = vapNOR
019200050909     C                   EVAL      x_vapNRV = vapNRV
019300060320     C                   EVAL      ar4TRC = 'M'
019400060317     C                   EVAL      ar4n14 = keyORM
019500060317     C     KEYar404      CHAIN     fiar404l
019600060317     C                   IF        %found(fiar404l)
019700060317     C                   DOW       not %eof(fiar404l)
019800060317     C     KEYarb01      CHAIN     fnarb01l
019900060317     C                   IF        %found(fnarb01l)
020000021015     C                   EVAL      flgCHK = 'S'
020100060412     C                   MOVEL(P)  arbRMA        fdxAWBNO
020200060320     C                   MOVE(P)   arbNCL        fdxPKGS
020300060519     C*
020400060519     C* Effettuo determinate considerazioni relativamente a peso e volume:
020500060519     C* - peso:
020600060519     C*
020700060519     C                   Z-ADD     arbPKF        wPesVDL
020800060519     C                   MOVEL(P)  wPesVDL       wPesoAlfa         7
020900060519     C                   EVALR     fdxPESO  = %subst(wPesoAlfa:1:6) + '.' +
021000060519     C                                        %subst(wPesoAlfa:7:1)
021100060519     C*
021200060519     C* - volume:
021300060519     C*                  IF        arbNCR = arbNCL
021400060519     C*                  Z-ADD     arbVLC        wVolVDL
021500060519     C*                  ELSE
021600060519     C*                  IF        arbVLC >= arbVLB
021700060519     C*                  Z-ADD     arbVLC        wVolVDL
021800060519     C*                  ELSE
021900060519     C*                  Z-ADD     arbVLB        wVolVDL
022000060519     C*                  ENDIF
022100060519     C*                  ENDIF
022200060519     C*
022300060519     C                   Z-ADD     arbVLF        wVolVDL
022400060519     C                   MOVEL(P)  wVolVDL       wVolumAlfa        5
022500060320     C                   EVALR     fdxVOLUM = %subst(wVolumAlfa:1:2) + '.' +
022600060320     C                                        %subst(wVolumAlfa:3:3)
022700060519     C*
022800021015     C                   WRITE     FNVAPxxx
022900021015     C                   ENDIF
023000060317     C     KEYar404      READE     fiar404l
023100021015     C                   ENDDO
023200060320     C*
023300060320     C* Se nn trovata bolla probabilmnete trattasi o d fase 600 o d fase 999...
023400060320     C* ...e cmq scarico lo stesso il buffer sin qui valorizzato
023500060320     C                   ELSE
023600060320     C                   WRITE     FNVAPxxx
023700020508     C                   ENDIF
023800020508     C*
023900020508     C                   ENDSR
024000020508
024100020508
024200020508
024300991027
024400991027      /TITLE Operazioni iniziali.
024500991027     C     *inzsr        BEGSR
024600991027     C*
024700991027     C     *ENTRY        PLIST
024800991027     C                   parm                    prmppt
024900991027     C     wrkesito      parm      wrkesito      prmesito
025000060320     C*
025100060320     C* IMPOSTA LA DATA CORRENTE
025200060320     C                   TIME                    N14              14 0
025300060320     C                   MOVE      N14           N8                8 0
025400060320     C                   MOVEL     N14           ORACOR            4
025500060320     C                   Z-ADD     N8            G08DAT
025600060320     C                   Z-ADD     *ZEROS        G08INV
025700060320     C                   MOVEL     '0'           G08ERR
025800060320     C                   CALL      'XSRDA8'
025900060320     C                   PARM                    WLBDA8
026000060320     C                   MOVE(P)   G08INV        DATCOR            8
026100020508     C*
026200020508     C* Definizione chiavi
026300020508     C*
026400060519     C* Chiave su FNARB01L - Completa
026500060317     C     KEYarb01      KLIST
026600060320     C                   KFLD                    ar4AAS
026700060320     C                   KFLD                    ar4LNP
026800060320     C                   KFLD                    ar4NRS
026900060320     C                   KFLD                    ar4NSP
027000020508     C*
027100060519     C* Chiave su FIAR401L - Completa
027200060317     C     KEYar404      KLIST
027300060317     C                   KFLD                    ar4TRC
027400060317     C                   KFLD                    ar4N14
027500060519     C* Chiave su TIVIR02L - Parziale
027600060320     C     KEYvir02_P    KLIST
027700060320     C                   KFLD                    virKSC
027800060320     C                   KFLD                    virTIP
027900060519     C*
028000060519     C* Chiave su TABEL00F - Completa
028100060519     C*    KEYtabel      KLIST
028200060519     C*                  KFLD                    tblKUT
028300060519     C*                  KFLD                    tblCOD
028400060519     C*                  KFLD                    tblKEY
028500991027     C*
028600991027     C                   ENDSR
