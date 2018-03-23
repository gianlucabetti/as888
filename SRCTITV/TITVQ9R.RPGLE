000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050413     FFNBLP01L  IF   E           K DISK
000500060215     FFIAR404L  IF   E           K DISK
000600060131     Ffnorn02l  IF   E           K DISK
000700050909     FTIVGDTMP  UF   E             DISK
000800050412     FTIVAWWWT  UF A E             DISK
000900031103
001000031103     D****
001100031103     D* DEFINIZIONE DS ESTERNE
001200031103     D****
001300050413     D DSBL4M        E DS
001400031103
001500050412     D prmppt          S             50
001600050412     D prmesito        S              1
001700050412     D wrkesito        S                   like(prmesito)
001800050909     D fnvapds       e ds                  extname(fnvap00t)
001900051116     D fnvapwwds     e ds                  extname(fnvapwwt) prefix(A_)
002000051116     D fnvabaar      e ds
002100060214     D fnblpds       e ds                  extname(fnblp00f)
002200051116
002300050412
002400031031
002500031031     C***********
002600030924     C* MAIN/
002700031031     C***********
002800050412     C                   EXSR      traduci
002900921023     C                   SETON                                        LR
003000991027
003100030924
003200030924
003300050412     C     traduci       BEGSR
003400991027     C*
003500050909     C                   READ      TIVGDTMP
003600050909     C                   dow       not %eof(TIVGDTMP)
003700050909     C                   eval      fnvapds = vgdDTA
003800051207     C*
003900051207     C* Inizializzo il buffer del file d out TIVAWWWT
004000051207     C                   clear                   TIVAW000
004100051207     C                   clear                   fnvapwwds
004200051207     C                   clear                   fnvabaar
004300060214     C                   clear                   fnblpds
004400060131     C*
004500060131     C                   MOVE      vapIDC        A_vapIDC
004600060131     C                   MOVE      vapPOE        A_vapPOE
004700060131     C                   MOVE      vapDPE        A_vapDPE
004800060131     C                   MOVE      vapNSR        A_vapNSR
004900060131     C                   MOVE      vapNOR        A_vapNOR
005000060131     C                   MOVE      vapNRV        A_vapNRV
005100060131     C                   MOVE      vapRFA        A_vapRFA
005200060131     C                   MOVE      vapPOG        A_vapPOG
005300060131     C                   MOVE      vapDPG        A_vapDPG
005400060131     C                   MOVE      vapDAE        A_vapDAE
005500060131     C                   MOVE      vapORE        A_vapORE
005600060131     C                   MOVE      vapFAR        A_vapFAR
005700060131     C                   MOVE      vapDFA        A_vapDFA
005800060131     C                   MOVE      vapCAR        A_vapCAR
005900060131     C                   MOVE      vapDCA        A_vapDCA
006000060131     C                   MOVE      vapNOT        A_vapNOT
006100060131     C*
006200060131     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
006300060131     C                   Z-ADD     177           wPosVAP           3 0
006400060131     C*
006500060131     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP
006600060131     C* (tutto il tracciato tranne l'ultimo campo ovvero le note)
006700060131     C                   EVAL      vawDTA = %subst(fnvapwwds:1:wPosVAP)
006800060131     C*
006900060131     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
007000060131     C     KEYorn2P      SETLL     fnorn02l
007100060131     C                   IF        %equal(fnorn02l)
007200060131     C* Inizializzo contatore x ritorno max 4 note
007300060131     C                   Z-ADD     1             wContaORN         1 0
007400060131     C* Lettura note
007500060131     C     KEYorn2P      READE     fnorn02l
007600060131     C                   DOW       not %eof(fnorn02l) AND
007700060131     C                             wContaORN <= 4
007800060131     C* Solo note con data/ora > data/ora esecuzione fase relativa
007900060131     C                   IF        orndai*1000000+ornori >=
008000060131     C                             vapdae*1000000+vapore
008100060131     C* Aggiungo al buffer del VAP la nota fase ORM corrente
008200060131     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
008300060131     C                             (35*(wContaORN-1))) + ornnot
008400060131     C                   ADD       1             wContaORN
008500060131     C                   ENDIF
008600060131     C*
008700060131     C     KEYorn2P      READE     fnorn02l
008800060131     C                   ENDDO
008900060131     C                   ENDIF
009000050413     C*
009100051201     C* Reperisco il dettaglio segnacolli solo x fasi 900 (ovvero ORM bollettato)
009200050413     C                   if        vapFAR = 900
009300050413     C*
009400050413     C* Dall'ORM aggancio l'estensione bolle partenza x reperire la bolla legata all'ORM corrente
009500050413     C                   clear                   DSBL4M
009600050413     C                   eval      §B4POE = VAPPOE
009700050413     C                   eval      §B4NSR = VAPNSR
009800050413     C                   eval      §B4NOR = VAPNOR
009900050413     C                   eval      §B4NRV = VAPNRV
010000050413     C*
010100060215     C                   eval      AR4TRC = 'M'
010200060215     C                   eval      AR4N14 = DSBL4M
010300060215     C     KEYar404M     chain     FIAR404L
010400060215     C                   if        %found(FIAR404L)
010500050413     C*
010600050413     C* Se ok => aggancio la bolla in partenza
010700050413     C     KEYblp01      chain     FNBLP01L
010800060214     C                   endif
010900060214     C*
011000060214     C* Indipendentemente dalla riuscita della chian (nel senso che se la trovo bene altrimenti
011100060214     C* ritorno solamente i dati delle fasi orm) valorizzo il buffer d output dei dati bolla
011200060214     C                   EXSR      valbufferBLP
011300060214     C*
011400060214     C* Valorizzo il buffer d output relativo ai dati provenienti dalle fasi ORM
011500060214     C                   EXSR      valbufferVAP
011600060131     C*
011700060131     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
011800060131     C                   MOVE      '.'           vawDTA
011900051116     C*
012000051116     C* Quindi scarico il buffer della bolla corrente
012100050413     C                   write     TIVAW000
012200060214     C*
012300060214     C* Se fase NN 900
012400051205     C                   else
012500051222     C*
012600051222     C* Se richiesto nei parametri d ingresso d NN restituire le fasi NN 900 => eseguo
012700051223     C                   if        %subst(prmppt:1:1) <> 'N'
012800060214     C*
012900060214     C* Indipendentemente dalla riuscita della chian (nel senso che se la trovo bene altrimenti
013000060214     C* ritorno solamente i dati delle fasi orm) valorizzo il buffer d output dei dati bolla
013100060214     C                   EXSR      valbufferBLP
013200060214     C*
013300060214     C* Valorizzo il buffer d output relativo ai dati provenienti dalle fasi ORM
013400060214     C                   EXSR      valbufferVAP
013500060131     C*
013600060131     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
013700060131     C                   MOVE      '.'           vawDTA
013800051201     C*
013900051205     C* Scrivo le altre fasi "semplicemente"
014000051201     C                   write     TIVAW000
014100051222     C                   endif
014200051201     C                   endif
014300031031     C*
014400050413     C* X ogni fase ORM eseguo storicizzazione e delete del file d input
014500050909     C                   DELETE    TIVGD000
014600031031     C*
014700050413     C* E proseguo con la lettura delle fasi da VAP
014800050909     C                   READ      TIVGDTMP
014900050412     C                   enddo
015000031031     C*
015100050412     C                   eval      wrkesito = '0'
015200031031     C*
015300031031     C                   ENDSR
015400051207
015500051207
015600051207
015700060214     C     valbufferBLP  BEGSR
015800051207     C*
015900051207     C                   MOVE      blpAAS        vhbAAS
016000051207     C                   MOVE      blpLNP        vhbLNP
016100051207     C                   MOVE      blpNRS        vhbNRS
016200051207     C                   MOVE      blpNSP        vhbNSP
016300051207     C                   MOVE      blpLNA        vhbLNA
016400051207     C                   MOVE      blpZNC        vhbZNC
016500051207     C                   MOVE      blpRMN        vhbRMN
016600051207     C                   MOVE      blpRMA        vhbRMA
016700051207     C                   MOVE      blpNCD        vhbNCD
016800051207     C                   MOVE      blpNCA        vhbNCA
016900051207     C                   MOVE      blpNCL        vhbNCL
017000051207     C*
017100051207     C                   ENDSR
017200060214
017300060214
017400060214
017500060214     C     valbufferVAP  BEGSR
017600060214     C*
017700060214     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
017800060214     C* finora valorizzato
017900060214     C                   Z-ADD     317           wPosVAP           3 0
018000060214     C*
018100060214     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP + 4 NOTE
018200060214     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP) +
018300060214     C                                      vhbAAS   +
018400060214     C                                      vhbLNP   +
018500060214     C                                      vhbNRS   +
018600060214     C                                      vhbNSP   +
018700060214     C                                      vhbLNA   +
018800060214     C                                      vhbZNC   +
018900060214     C                                      vhbRMN   +
019000060214     C                                      vhbRMA   +
019100060214     C                                      vhbNCD   +
019200060214     C                                      vhbNCA   +
019300060214     C                                      vhbNCL
019400060214     C*
019500060214     C                   ENDSR
019600050412
019700050412
019800050412
019900991027      /TITLE Operazioni iniziali.
020000991027     C     *inzsr        BEGSR
020100991027     C*
020200991027     C     *ENTRY        PLIST
020300991027     C                   parm                    prmppt
020400991027     C     wrkesito      parm      wrkesito      prmesito
020500030709     C*
020600030709     C* Definizione chiavi
020700030924     C*
020800060215     C* Chiave su FIAR404L - Completa
020900060215     C     KEYar404M     KLIST
021000060215     C                   KFLD                    AR4TRC
021100060215     C                   KFLD                    AR4N14
021200050413     C* Chiave su FNBLP01L - Completa
021300050413     C     KEYblp01      KLIST
021400060215     C                   KFLD                    AR4AAS
021500060215     C                   KFLD                    AR4LNP
021600060215     C                   KFLD                    AR4NRS
021700060215     C                   KFLD                    AR4NSP
021800060131     C* Chiave su FNORN02L - Parziale
021900060131     C     KEYorn2P      KLIST
022000060131     C                   KFLD                    VAPPOE
022100060131     C                   KFLD                    VAPNSR
022200060131     C                   KFLD                    VAPNOR
022300060131     C                   KFLD                    VAPNRV
022400060131     C                   KFLD                    VAPFAR
022500991027     C*
022600991027     C                   ENDSR
