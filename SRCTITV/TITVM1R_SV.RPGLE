000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050412     FTITAS30C  IF   E           K DISK
000500050412     FTITA430C  IF   E           K DISK
000600050412     FTITAH30C  IF   E           K DISK
000700050412     FFNORM01L  IF   E           K DISK
000800050412     FFNVAB00T  UF   E             DISK
000900031031     FFNVAB00S  O    E             DISK    rename(fnvab000:fnvabsss)
001000050412     FTIVAWWWT  UF A E             DISK
001100031103
001200031103     D****
001300031103     D* DEFINIZIONE DS ESTERNE
001400031103     D****
001500050412     D DSBL4M        E DS
001600050412     D DSBLD7        E DS
001700050412     D TRUL28DS      E DS
001800031103
001900050412     D prmppt          S             50
002000050412     D prmesito        S              1
002100050412     D wrkesito        S                   like(prmesito)
002200050412
002300031103     D****
002400031103     D* VARIABILI DI WRK
002500031103     D****
002600050412     D wORMRFA         s                   like(ormRFA) inz(*blanks)
002700050412     D wDATRIT         s                   like(tasDRT) inz(*zeros)
002800050412     D wBARCODE        s             18    inz(*blanks)
002900050412     D wYESORM         s               N   inz(*off)
003000050412     D i               s              5  0 inz(*zeros)
003100050412     D parDelimFld     s              1
003200050412     D parDelimTXT     s              1
003300050412     D parRFA          s              1
003400031031
003500031031
003600031031     C***********
003700030924     C* MAIN/
003800031031     C***********
003900050412     C                   EXSR      traduci
004000921023     C                   SETON                                        LR
004100991027
004200030924
004300030924
004400050412     C     traduci       BEGSR
004500991027     C*
004600050412     C                   read      FNVAB00T
004700050412     C                   dow       not %eof(FNVAB00T)
004800050412     C*
004900050412     C* Inizializzo il buffer del file d out TIVAWWWT
005000050412     C                   clear                   TIVAW000
005100050412     C*
005200050412     C* Inizializzo le variabili d wrk
005300050412     C                   eval      wYESORM = *off
005400050412     C                   eval      wORMRFA = *blanks
005500050412     C                   eval      wDATRIT = *zeros
005600050412     C*
005700050412     C* Se richiesto nei parametri d traduzione tento di reperire l'ORM a cui la bolla è legata
005800050412     C                   if        parRFA = 'O'
005900050412     C* ... x cui aggancio il file riferimenti bolle con tipo record 'M' in cui viene
006000050412     C* ... memorizzata la chiave dell'ORM legato alla bolla
006100050412     C                   eval      ta4TRC = 'M'
006200050412     C     KEYta430M     chain     tita430c
006300050412     C                   if        %found(tita430c)
006400050412     C                   if        ta4NOT <> *blanks
006500050412     C* ... se trovato il record e le note sono valorizzate (ovvero è presente la chiave ORM)
006600050412     C* ...... accendo l'indicatore che mi indica la presenza dell'ORM legato
006700050412     C                   eval      wYESORM = *on
006800050412     C* ...... leggo il file degli ORM x reperire il riferimento alfabetico originale
006900050412     C                   eval      dsbl4m = ta4NOT
007000050412     C     KEYorm01      chain     fnorm01l
007100050412     C                   if        %found(fnorm01l)
007200050412     C                   eval      wORMRFA = ormRFA
007300050412     C                   endif
007400050412     C                   endif
007500050412     C                   endif
007600050412     C                   endif
007700050412     C*
007800050412     C* Aggancio la bolla d sede x reperire la data di ritiro
007900050412     C     KEYtas30      chain     titas30c
008000050412     C                   if        %found(titas30c)
008100050412     C                   eval      wDATRIT = tasDRT
008200050412     C                   endif
008300050412     C*
008400050412     C* Se nn reperito il riferimento mittente alfabetico dall'ORM considero quello della bolla
008500050412     C                   if        wORMRFA = *blanks
008600050412     C                   eval      wORMRFA = vabRMA
008700050412     C                   endif
008800050412     C*
008900050412     C* Se segnacolli sequenziali ciclo sulle bolle di sede TITAS
009000050412     C                   if        tasFNS <> 'S'
009100050412     C                   for       i=tasNCD TO tasNCD+tasNCL
009200050412     C                   clear                   DSBLD7
009300050412     C                   eval      §LCLNP = tasLNP
009400050412     C                   eval      §LCLNA = tasLNA
009500050412     C                   eval      §LCNRS = tasNRS
009600050412     C                   eval      §LCNCD = i
009700050412     C                   eval      §LCZSC = tasZNC
009800120307     C* X ogni segnacollo costruisco il barcode standard BRT
009900050412     C                   exsr      calbarcode
010000050412     C                   eval      VAWDTA = parDelimTXT +
010100050412     C                                          wORMRFA +
010200050412     C                                      parDelimTXT +
010300050412     C                                      parDelimFld +
010400050412     C                                %editc(tasNCL:'Z')+
010500050412     C                                      parDelimFld +
010600050412     C                               %editc(wDATRIT:'Z')+
010700050412     C                                      parDelimFld +
010800050412     C                                      parDelimTXT +
010900050412     C                                         wBARCODE +
011000050412     C                                      parDelimTXT
011100050412     C* Quindi scarico il buffer del collo corrente
011200050412     C                   write     TIVAW000
011300050412     C*
011400050412     C                   endfor
011500050412     C*
011600050412     C                   else
011700050412     C*
011800050412     C* Se segnacolli NN sequenziali aggancio il dettaglio colli e ciclo x ogni segnacollo presente
011900050412     C                   eval      tahTRC = 'C'
012000050412     C     KEYtah30C     setll     titah30c
012100050412     C                   if        %equal(titah30c)
012200050412     C     KEYtah30C     reade     titah30c
012300050412     C                   dow       not %eof(titah30c)
012400120307     C* X ogni segnacollo costruisco il barcode standard BRT
012500050412     C                   clear                   DSBLD7
012600050412     C                   eval      §LCLNP = tasLNP
012700050412     C                   eval      §LCLNA = tasLNA
012800050412     C                   eval      §LCNRS = tasNRS
012900050412     C                   movel     tahNOT        §LCNCD
013000050412     C                   eval      §LCZSC = tasZNC
013100120307     C* X ogni segnacollo costruisco il barcode standard BRT
013200050412     C                   exsr      calbarcode
013300050412     C                   eval      VAWDTA = parDelimTXT +
013400050412     C                                          wORMRFA +
013500050412     C                                      parDelimTXT +
013600050412     C                                      parDelimFld +
013700050412     C                                %editc(tasNCL:'Z')+
013800050412     C                                      parDelimFld +
013900050412     C                               %editc(wDATRIT:'Z')+
014000050412     C                                      parDelimFld +
014100050412     C                                      parDelimTXT +
014200050412     C                                         wBARCODE +
014300050412     C                                      parDelimTXT
014400050412     C* Quindi scarico il buffer del collo corrente
014500050412     C                   write     TIVAW000
014600050412     C* E proseguo con la lettura dei segnacolli della bolla
014700050412     C     KEYtah30C     reade     titah30c
014800050412     C                   enddo
014900050412     C*
015000050412     C                   endif
015100050412     C                   endif
015200031031     C*
015300050412     C* X ogni bolla eseguo storicizzazione e delete del file d input
015400050412     C                   write     FNVABsss
015500050412     C                   delete    FNVAB000
015600031031     C*
015700050412     C* E proseguo con la lettura delle bolle da VAB
015800050412     C                   read      FNVAB00T
015900050412     C                   enddo
016000031031     C*
016100050412     C                   eval      wrkesito = '0'
016200031031     C*
016300031031     C                   ENDSR
016400050412
016500050412
016600050412
016700050412     C     calbarcode    BEGSR
016800050412     C*
016900120307     C* Inizializzo la ds d procedura x calcolo barcode standard BRT e variabili d wrk
017000050412     C                   clear                   TRUL28DS
017100050412     C                   clear                   wBARCODE
017200050412     C*
017300050412     C* Valorizzo il buffer d input
017400050412     C                   eval      I28TLA = 'E'
017500050412     C                   eval      I28MOD = 'BAR'
017600050412     C                   eval      I28COD = DSBLD7
017700050412     C                   if        O28ERR = *blanks
017800050412     C                   eval      wBARCODE = O28COD
017900050412     C                   endif
018000050412     C*
018100050412     C                   ENDSR
018200031031
018300031031
018400031031
018500031031
018600991027
018700991027      /TITLE Operazioni iniziali.
018800991027     C     *inzsr        BEGSR
018900991027     C*
019000991027     C     *ENTRY        PLIST
019100991027     C                   parm                    prmppt
019200991027     C     wrkesito      parm      wrkesito      prmesito
019300050412     C*
019400050412     C* Imposto i flag d procedura dai parametri d traduzione ricevuti in input
019500050412     C                   eval      parRFA = %subst(prmppt:1:1)
019600050412     C                   eval      parDelimTXT = %subst(prmppt:2:1)
019700050412     C                   eval      parDelimFld = %subst(prmppt:3:1)
019800030709     C*
019900030709     C* Definizione chiavi
020000030924     C*
020100050412     C* Chiave su FNORM01L - Completa
020200050412     C     KEYorm01      KLIST
020300050412     C                   KFLD                    §B4POE
020400050412     C                   KFLD                    §B4NSR
020500050412     C                   KFLD                    §B4NOR
020600050412     C                   KFLD                    §B4NRV
020700050412     C* Chiave su TITA430C - Completa
020800050412     C     KEYta430M     KLIST
020900031031     C                   KFLD                    vabAAS
021000031031     C                   KFLD                    vabLNP
021100031031     C                   KFLD                    vabNRS
021200031031     C                   KFLD                    vabNSP
021300050412     C                   KFLD                    TA4TRC
021400050412     C* Chiave su TITAS30C - Parziale
021500050412     C     KEYtas30      KLIST
021600050412     C                   KFLD                    vabAAS
021700050412     C                   KFLD                    vabLNP
021800050412     C                   KFLD                    vabNRS
021900050412     C                   KFLD                    vabNSP
022000050412     C* Chiave su TITAH30C - Completa
022100050412     C     KEYtah30C     KLIST
022200050412     C                   KFLD                    vabAAS
022300050412     C                   KFLD                    vabLNP
022400050412     C                   KFLD                    vabNRS
022500050412     C                   KFLD                    vabNSP
022600050412     C                   KFLD                    TAHTRC
022700991027     C*
022800991027     C                   ENDSR
