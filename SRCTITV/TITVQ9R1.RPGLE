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
001800110712     D fnvabaar      e ds
001900050909     D fnvapds       e ds                  extname(fnvap00t)
002000051116     D fnvapwwds     e ds                  extname(fnvapwwt) prefix(A_)
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
004200060214     C                   clear                   fnblpds
004300060131     C*
004400060131     C                   MOVE      vapIDC        A_vapIDC
004500060131     C                   MOVE      vapPOE        A_vapPOE
004600060131     C                   MOVE      vapDPE        A_vapDPE
004700060131     C                   MOVE      vapNSR        A_vapNSR
004800060131     C                   MOVE      vapNOR        A_vapNOR
004900060131     C                   MOVE      vapNRV        A_vapNRV
005000060131     C                   MOVE      vapRFA        A_vapRFA
005100060131     C                   MOVE      vapPOG        A_vapPOG
005200060131     C                   MOVE      vapDPG        A_vapDPG
005300060131     C                   MOVE      vapDAE        A_vapDAE
005400060131     C                   MOVE      vapORE        A_vapORE
005500060131     C                   MOVE      vapFAR        A_vapFAR
005600060131     C                   MOVE      vapDFA        A_vapDFA
005700060131     C                   MOVE      vapCAR        A_vapCAR
005800060131     C                   MOVE      vapDCA        A_vapDCA
005900060131     C                   MOVE      vapNOT        A_vapNOT
006000060131     C*
006100060131     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
006200110712     C                   Z-ADD     227           wPosVAP           3 0
006300060131     C*
006400060131     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP
006500060131     C* (tutto il tracciato tranne l'ultimo campo ovvero le note)
006600060131     C                   EVAL      vawDTA = %subst(fnvapwwds:1:wPosVAP)
006700060131     C*
006800060131     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
006900060131     C     KEYorn2P      SETLL     fnorn02l
007000060131     C                   IF        %equal(fnorn02l)
007100060131     C* Inizializzo contatore x ritorno max 4 note
007200060131     C                   Z-ADD     1             wContaORN         1 0
007300060131     C* Lettura note
007400060131     C     KEYorn2P      READE     fnorn02l
007500060131     C                   DOW       not %eof(fnorn02l) AND
007600060131     C                             wContaORN <= 4
007700060131     C* Solo note con data/ora > data/ora esecuzione fase relativa
007800060131     C                   IF        orndai*1000000+ornori >=
007900060131     C                             vapdae*1000000+vapore
008000060131     C* Aggiungo al buffer del VAP la nota fase ORM corrente
008100060131     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
008200060131     C                             (35*(wContaORN-1))) + ornnot
008300060131     C                   ADD       1             wContaORN
008400060131     C                   ENDIF
008500060131     C*
008600060131     C     KEYorn2P      READE     fnorn02l
008700060131     C                   ENDDO
008800060131     C                   ENDIF
008900050413     C*
009000051201     C* Reperisco il dettaglio segnacolli solo x fasi 900 (ovvero ORM bollettato)
009100050413     C                   if        vapFAR = 900
009200050413     C*
009300050413     C* Dall'ORM aggancio l'estensione bolle partenza x reperire la bolla legata all'ORM corrente
009400050413     C                   clear                   DSBL4M
009500050413     C                   eval      §B4POE = VAPPOE
009600050413     C                   eval      §B4NSR = VAPNSR
009700050413     C                   eval      §B4NOR = VAPNOR
009800050413     C                   eval      §B4NRV = VAPNRV
009900050413     C*
010000060215     C                   eval      AR4TRC = 'M'
010100060215     C                   eval      AR4N14 = DSBL4M
010200060215     C     KEYar404M     chain     FIAR404L
010300060215     C                   if        %found(FIAR404L)
010400050413     C*
010500050413     C* Se ok => aggancio la bolla in partenza
010600050413     C     KEYblp01      chain     FNBLP01L
010700060214     C                   endif
010800060214     C*
010900060214     C* Indipendentemente dalla riuscita della chian (nel senso che se la trovo bene altrimenti
011000060214     C* ritorno solamente i dati delle fasi orm) valorizzo il buffer d output dei dati bolla
011100060214     C                   EXSR      valbufferBLP
011200060214     C*
011300060214     C* Valorizzo il buffer d output relativo ai dati provenienti dalle fasi ORM
011400060214     C                   EXSR      valbufferVAP
011500051116     C*
011600051116     C* Quindi scarico il buffer della bolla corrente
011700050413     C                   write     TIVAW000
011800060214     C*
011900060214     C* Se fase NN 900
012000051205     C                   else
012100051222     C*
012200051222     C* Se richiesto nei parametri d ingresso d NN restituire le fasi NN 900 => eseguo
012300051223     C                   if        %subst(prmppt:1:1) <> 'N'
012400060214     C*
012500060214     C* Indipendentemente dalla riuscita della chian (nel senso che se la trovo bene altrimenti
012600060214     C* ritorno solamente i dati delle fasi orm) valorizzo il buffer d output dei dati bolla
012700060214     C                   EXSR      valbufferBLP
012800060214     C*
012900060214     C* Valorizzo il buffer d output relativo ai dati provenienti dalle fasi ORM
013000060214     C                   EXSR      valbufferVAP
013100051201     C*
013200051205     C* Scrivo le altre fasi "semplicemente"
013300051201     C                   write     TIVAW000
013400051222     C                   endif
013500051201     C                   endif
013600031031     C*
013700050413     C* X ogni fase ORM eseguo storicizzazione e delete del file d input
013800050909     C                   DELETE    TIVGD000
013900031031     C*
014000050413     C* E proseguo con la lettura delle fasi da VAP
014100050909     C                   READ      TIVGDTMP
014200050412     C                   enddo
014300031031     C*
014400050412     C                   eval      wrkesito = '0'
014500031031     C*
014600031031     C                   ENDSR
014700051207
014800051207
014900051207
015000060214     C     valbufferBLP  BEGSR
015100051207     C*
015200051207     C                   MOVE      blpAAS        vhbAAS
015300051207     C                   MOVE      blpLNP        vhbLNP
015400051207     C                   MOVE      blpNRS        vhbNRS
015500051207     C                   MOVE      blpNSP        vhbNSP
015600051207     C                   MOVE      blpLNA        vhbLNA
015700051207     C                   MOVE      blpZNC        vhbZNC
015800051207     C                   MOVE      blpRMN        vhbRMN
015900051207     C                   MOVE      blpRMA        vhbRMA
016000051207     C                   MOVE      blpNCD        vhbNCD
016100051207     C                   MOVE      blpNCA        vhbNCA
016200051207     C                   MOVE      blpNCL        vhbNCL
016300051207     C*
016400051207     C                   ENDSR
016500060214
016600060214
016700060214
016800060214     C     valbufferVAP  BEGSR
016900060214     C*
017000060214     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
017100060214     C* finora valorizzato
017200060214     C                   Z-ADD     317           wPosVAP           3 0
017300060214     C*
017400060214     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP + 4 NOTE
017500060214     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP) +
017600060214     C                                      vhbAAS   +
017700060214     C                                      vhbLNP   +
017800060214     C                                      vhbNRS   +
017900060214     C                                      vhbNSP   +
018000060214     C                                      vhbLNA   +
018100060214     C                                      vhbZNC   +
018200060214     C                                      vhbRMN   +
018300060214     C                                      vhbRMA   +
018400060214     C                                      vhbNCD   +
018500060214     C                                      vhbNCA   +
018600071112     C                                      vhbNCL   +
018700071112     C                                      '.'
018800060214     C*
018900060214     C                   ENDSR
019000050412
019100050412
019200050412
019300991027      /TITLE Operazioni iniziali.
019400991027     C     *inzsr        BEGSR
019500991027     C*
019600991027     C     *ENTRY        PLIST
019700991027     C                   parm                    prmppt
019800991027     C     wrkesito      parm      wrkesito      prmesito
019900030709     C*
020000030709     C* Definizione chiavi
020100030924     C*
020200060215     C* Chiave su FIAR404L - Completa
020300060215     C     KEYar404M     KLIST
020400060215     C                   KFLD                    AR4TRC
020500060215     C                   KFLD                    AR4N14
020600050413     C* Chiave su FNBLP01L - Completa
020700050413     C     KEYblp01      KLIST
020800060215     C                   KFLD                    AR4AAS
020900060215     C                   KFLD                    AR4LNP
021000060215     C                   KFLD                    AR4NRS
021100060215     C                   KFLD                    AR4NSP
021200060131     C* Chiave su FNORN02L - Parziale
021300060131     C     KEYorn2P      KLIST
021400060131     C                   KFLD                    VAPPOE
021500060131     C                   KFLD                    VAPNSR
021600060131     C                   KFLD                    VAPNOR
021700060131     C                   KFLD                    VAPNRV
021800060131     C                   KFLD                    VAPFAR
021900991027     C*
022000991027     C                   ENDSR
