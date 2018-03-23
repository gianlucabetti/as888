000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050413     FFNBLP01L  IF   E           K DISK
000500060215     FFIAR404L  IF   E           K DISK
000600050909     FTIVGDTMP  UF   E             DISK
000700050412     FTIVAWWWT  UF A E             DISK
000800031103
000900031103     D****
001000031103     D* DEFINIZIONE DS ESTERNE
001100031103     D****
001200050413     D DSBL4M        E DS
001300031103
001400050412     D prmppt          S             50
001500050412     D prmesito        S              1
001600050412     D wrkesito        S                   like(prmesito)
001700050909     D fnvapds       e ds                  extname(fnvap00t)
001800051116     D fnvapwwds     e ds                  extname(fnvapwwt) prefix(A_)
001900051116     D fnvabaar      e ds
002000051116
002100060306     D*------------------
002200060306     D* DS RIDEFINIZIONE CHIAVE ALFA BOLLA ORIGINALE
002300060306     D*------------------
002400060306     D                 DS                  INZ
002500060306     D  wvabAAS                1      4  0
002600060306     D  wvabLNP                5      7  0
002700060306     D  wvabNRS                8      9  0
002800060306     D  wvabNSP               10     16  0
002900060306     D  vabKEY                 1     16
003000050412
003100031031
003200031031     C***********
003300030924     C* MAIN/
003400031031     C***********
003500050412     C                   EXSR      traduci
003600921023     C                   SETON                                        LR
003700991027
003800030924
003900030924
004000050412     C     traduci       BEGSR
004100991027     C*
004200050909     C                   READ      TIVGDTMP
004300050909     C                   dow       not %eof(TIVGDTMP)
004400050909     C                   eval      fnvapds = vgdDTA
004500051207     C*
004600051207     C* Inizializzo il buffer del file d out TIVAWWWT
004700051207     C                   clear                   TIVAW000
004800051207     C                   clear                   fnvapwwds
004900051207     C                   clear                   fnvabaar
005000051207     C                   clear                   fnblp000
005100050413     C*
005200051201     C* Reperisco il dettaglio segnacolli solo x fasi 900 (ovvero ORM bollettato)
005300050413     C                   if        vapFAR = 900
005400050413     C*
005500050413     C* Dall'ORM aggancio l'estensione bolle partenza x reperire la bolla legata all'ORM corrente
005600050413     C                   clear                   DSBL4M
005700050413     C                   eval      §B4POE = VAPPOE
005800050413     C                   eval      §B4NSR = VAPNSR
005900050413     C                   eval      §B4NOR = VAPNOR
006000050413     C                   eval      §B4NRV = VAPNRV
006100050413     C*
006200060215     C                   eval      AR4TRC = 'M'
006300060215     C                   eval      AR4N14 = DSBL4M
006400060215     C     KEYar404M     chain     FIAR404L
006500060215     C                   if        %found(FIAR404L)
006600050413     C*
006700050413     C* Se ok => aggancio la bolla in partenza
006800050413     C     KEYblp01      chain     FNBLP01L
006900050413     C                   if        %found(FNBLP01L)
007000051116     C*
007100051207     C* Valorizzo il buffer d output
007200051207     C                   EXSR      valbuffer
007300051116     C*
007400051116     C* Quindi scarico il buffer della bolla corrente
007500050413     C                   write     TIVAW000
007600050413     C*
007700050413     C                   endif
007800050413     C                   endif
007900060306     C                   else
008000051222     C*
008100060306     C* Se richiesto nei parametri d ingresso d restituire solo le fasi 900 => salto tutto il resto
008200060306     C                   if        %subst(prmppt:1:1) = 'N'
008300060306     C                   else
008400060306     C*
008500060306     C* Se richiesto nei parametri d ingresso d restituire anche le fasi 999 => eseguo
008600060306     C                   if        %subst(prmppt:1:1) = 'C'
008700060306     C                   if        vapFAR = 999
008800060306     C
008900060306     C                   eval      vabKEY = '20' + %subst(vapRFA:2:14)
009000060306     C*
009100060306     C* Quindi aggancio in partenza la bolla "originale RC"
009200060306     C     KEYblp01_2    chain     FNBLP01L
009300060306     C                   if        %found(FNBLP01L)
009400060306     C*
009500060306     C* Valorizzo il buffer d output
009600060306     C                   EXSR      valbuffer
009700060306     C*
009800060306     C* Scrivo le altre fasi "semplicemente"
009900060306     C                   write     TIVAW000
010000060306     C                   endif
010100060306     C                   endif
010200060306     C*
010300060306     C* Se nn specificato nulla => restituisco tutte le fasi
010400060306     C                   else
010500051207     C*
010600051207     C* Valorizzo il buffer d output
010700051207     C                   EXSR      valbuffer
010800051201     C*
010900051205     C* Scrivo le altre fasi "semplicemente"
011000051201     C                   write     TIVAW000
011100051222     C                   endif
011200051201     C                   endif
011300060306     C                   endif
011400031031     C*
011500050413     C* X ogni fase ORM eseguo storicizzazione e delete del file d input
011600050909     C                   DELETE    TIVGD000
011700031031     C*
011800050413     C* E proseguo con la lettura delle fasi da VAP
011900050909     C                   READ      TIVGDTMP
012000050412     C                   enddo
012100031031     C*
012200050412     C                   eval      wrkesito = '0'
012300031031     C*
012400031031     C                   ENDSR
012500051207
012600051207
012700051207
012800051207     C     valbuffer     BEGSR
012900051207     C*
013000051207     C* ...ciclo sulle bolle in partenza x reperire i dati segnacolli necessari alla
013100051207     C* composizione del barcode Bartolini
013200051207     C                   MOVE      vapIDC        A_vapIDC
013300051207     C                   MOVE      vapPOE        A_vapPOE
013400051207     C                   MOVE      vapDPE        A_vapDPE
013500051207     C                   MOVE      vapNSR        A_vapNSR
013600051207     C                   MOVE      vapNOR        A_vapNOR
013700051207     C                   MOVE      vapNRV        A_vapNRV
013800051207     C                   MOVE      vapRFA        A_vapRFA
013900051207     C                   MOVE      vapPOG        A_vapPOG
014000051207     C                   MOVE      vapDPG        A_vapDPG
014100051207     C                   MOVE      vapDAE        A_vapDAE
014200051207     C                   MOVE      vapORE        A_vapORE
014300051207     C                   MOVE      vapFAR        A_vapFAR
014400051207     C                   MOVE      vapDFA        A_vapDFA
014500051207     C                   MOVE      vapCAR        A_vapCAR
014600051207     C                   MOVE      vapDCA        A_vapDCA
014700051207     C                   MOVE      vapNOT        A_vapNOT
014800051207     C                   MOVE      blpAAS        vhbAAS
014900051207     C                   MOVE      blpLNP        vhbLNP
015000051207     C                   MOVE      blpNRS        vhbNRS
015100051207     C                   MOVE      blpNSP        vhbNSP
015200051207     C                   MOVE      blpLNA        vhbLNA
015300051207     C                   MOVE      blpZNC        vhbZNC
015400051207     C                   MOVE      blpRMN        vhbRMN
015500051207     C                   MOVE      blpRMA        vhbRMA
015600051207     C                   MOVE      blpNCD        vhbNCD
015700051207     C                   MOVE      blpNCA        vhbNCA
015800051207     C                   MOVE      blpNCL        vhbNCL
015900051207     C*
016000051207     C                   EVAL      vawDTA = A_vapIDC +
016100051207     C                                      A_vapPOE +
016200051207     C                                      A_vapDPE +
016300051207     C                                      A_vapNSR +
016400051207     C                                      A_vapNOR +
016500051207     C                                      A_vapNRV +
016600051207     C                                      A_vapRFA +
016700051207     C                                      A_vapPOG +
016800051207     C                                      A_vapDPG +
016900051207     C                                      A_vapDAE +
017000051207     C                                      A_vapORE +
017100051207     C                                      A_vapFAR +
017200051207     C                                      A_vapDFA +
017300051207     C                                      A_vapCAR +
017400051207     C                                      A_vapDCA +
017500051207     C                                      A_vapNOT +
017600051207     C                                      vhbAAS   +
017700051207     C                                      vhbLNP   +
017800051207     C                                      vhbNRS   +
017900051207     C                                      vhbNSP   +
018000051207     C                                      vhbLNA   +
018100051207     C                                      vhbZNC   +
018200051207     C                                      vhbRMN   +
018300051207     C                                      vhbRMA   +
018400051207     C                                      vhbNCD   +
018500051207     C                                      vhbNCA   +
018600051207     C                                      vhbNCL
018700051207     C*
018800051207     C                   ENDSR
018900050412
019000050412
019100050412
019200991027      /TITLE Operazioni iniziali.
019300991027     C     *inzsr        BEGSR
019400991027     C*
019500991027     C     *ENTRY        PLIST
019600991027     C                   parm                    prmppt
019700991027     C     wrkesito      parm      wrkesito      prmesito
019800030709     C*
019900030709     C* Definizione chiavi
020000030924     C*
020100060215     C* Chiave su FIAR404L - Completa
020200060215     C     KEYar404M     KLIST
020300060215     C                   KFLD                    AR4TRC
020400060215     C                   KFLD                    AR4N14
020500050413     C* Chiave su FNBLP01L - Completa
020600050413     C     KEYblp01      KLIST
020700060215     C                   KFLD                    AR4AAS
020800060215     C                   KFLD                    AR4LNP
020900060215     C                   KFLD                    AR4NRS
021000060215     C                   KFLD                    AR4NSP
021100060306     C* Chiave su FNBLP01L - Completa - 2°
021200060306     C     KEYblp01_2    KLIST
021300060306     C                   KFLD                    wvabAAS
021400060306     C                   KFLD                    wvabLNP
021500060306     C                   KFLD                    wvabNRS
021600060306     C                   KFLD                    wvabNSP
021700991027     C*
021800991027     C                   ENDSR
