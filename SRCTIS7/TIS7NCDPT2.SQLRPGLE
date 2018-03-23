000100010925      *PARMS CLOSQLCSR(*ENDMOD)
000200010925      *PARMS DFTACTGRP(*YES)
000300010925      *PARMS DYNUSRPRF(*OWNER)
000400010726     H*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600001130      *------------------------------------------------------------------------*
000700000914     FTNTBE01L  IF   E           K DISK
000800010726     FTTVACPT   UF A E           K DISK    RENAME(TTVACPT:TTVACPT000)
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000918     D*-------------------
001300000918     D* SCHIERE RAGGRUPPAMENTI SERIE CLIENTI POSTE
001400000918     D*-------------------
001500000918     D RSRNRS          S              2  0 DIM(500)                             SERIE
001600000918     D RSRDES          S             30    DIM(500)                             DESCRIZIONE
001700000918     D                                     varying
001800000918     D RSRRGR          S              3    DIM(500)                             RAGGRUPPAMENTO
001900000918     D                                     varying
002000010723     D*-------------------
002100010723     D* DS RAGGRUPPAMENTI SERIE CLIENTI POSTE
002200010723     D*-------------------
002300010723     D DRSR          E DS
002400010723     D*-------------------
002500010723     D* DS RIDEFINIZIONE SEGNACOLLI BARTOLINI
002600001122     D*-------------------
002700010723     D                 DS                  INZ
002800010723     D  SCLLNP                 1      3  0
002900010723     D  SCLNRS                 4      5  0
003000010723     D  SCLNSP                 6     12  0
003100010723     D  SCLTOT                 1     12  0
003200010723     D*-------------------
003300010723     D* DS DI LETTURA SNAPSHOT ARCHIVIO DI INPUT VIA SQL
003400010723     D*-------------------
003500010726     D  DSFNVACPT    E DS                  EXTNAME(FNVACPT)
003600000914     D*------------------------------------------------------------------------*
003700010723     D* VARIABILI DI WRK
003800000914     D*------------------------------------------------------------------------*
003900010723     D depnfi          s                   like(vacnfi) inz(*blanks)
004000030304     D wrkXX8          s                   like(vacxx8) inz(*blanks)
004100000000     C*------------------------------------------------------------------------*
004200000000     C* MAIN LINE
004300000000     C*------------------------------------------------------------------------*
004400000000     C*
004500000920     C                   exsr      procedi
004600030304     C                   exsr      procediXX8
004700000914     C*
004800000914     C                   SETON                                        LR
004900000914     C*------------------------------------------------------------------------*
005000000914     C* PROCEDI - Routine di elaborazione
005100000914     C*------------------------------------------------------------------------*
005200000914     C     PROCEDI       BEGSR
005300030304     C*
005400030304     C                   clear                   depnfi
005500030304     C                   clear                   wrkXX8
005600030304     C                   clear                   ttvacpt000
005700001130     C*
005800010726     C*
005900000914     C/EXEC SQL
006000021022     C+ DECLARE C1 CURSOR FOR SELECT * FROM fnvacpt WHERE vacidl = :IDL
006100030304     C+ AND vacxx8 <> '1'
006200021022     C+ ORDER BY vacnfi FOR READ ONLY
006300000914     C/END-EXEC
006400000914     C
006500000914     C/EXEC SQL
006600000914     C+ open C1
006700000914     C/END-EXEC
006800000914     C
006900000914     C/EXEC SQL
007000010723     C+ Fetch C1 into :dsfnvacpt
007100000914     C/END-EXEC
007200000914     C*
007300030516     C                   if        sqlcod <> 100
007400001130     C                   dow       sqlcod = *zeros
007500010723     C*
007600010723     C                   exsr      creare
007700000914     C*
007800000914     C/EXEC SQL
007900010723     C+ Fetch C1 into :dsfnvacpt
008000000914     C/END-EXEC
008100010723     C*
008200001130     C                   enddo
008300010723     C*
008400010723     C                   exsr      creare
008500030516     C                   endif
008600000914     C*
008700000914     C/EXEC SQL
008800000914     C+ close C1
008900000914     C/END-EXEC
009000010726     C*
009100000920     C*
009200000914     C                   ENDSR
009300030304     C*------------------------------------------------------------------------*
009400030304     C* PROCEDIXX8 - Routine di elaborazione
009500030304     C*------------------------------------------------------------------------*
009600030304     C     PROCEDIXX8    BEGSR
009700030304     C*
009800030304     C                   clear                   depnfi
009900030304     C                   eval      wrkXX8 = '1'
010000030304     C                   clear                   ttvacpt000
010100030304     C*
010200030304     C*
010300030304     C/EXEC SQL
010400030304     C+ DECLARE C2 CURSOR FOR SELECT * FROM fnvacpt WHERE vacidl = :IDL
010500030304     C+ AND vacxx8 = '1'
010600030304     C+ ORDER BY vacnfi FOR READ ONLY
010700030304     C/END-EXEC
010800030304     C
010900030304     C/EXEC SQL
011000030304     C+ open C2
011100030304     C/END-EXEC
011200030304     C
011300030304     C/EXEC SQL
011400030304     C+ Fetch C2 into :dsfnvacpt
011500030304     C/END-EXEC
011600030304     C*
011700030516     C                   if        sqlcod <> 100
011800030304     C                   dow       sqlcod = *zeros
011900030304     C*
012000030304     C                   exsr      creare
012100030304     C*
012200030304     C/EXEC SQL
012300030304     C+ Fetch C2 into :dsfnvacpt
012400030304     C/END-EXEC
012500030304     C*
012600030304     C                   enddo
012700030304     C*
012800030304     C                   exsr      creare
012900030516     C                   endif
013000030304     C*
013100030304     C/EXEC SQL
013200030304     C+ close C2
013300030304     C/END-EXEC
013400030304     C*
013500030304     C*
013600030304     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800010723     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
013900000000     C*------------------------------------------------------------------------*
014000000000     C     CREARE        BEGSR
014100030304     C*
014200030304     C* Valorizzo subito il flag che indica se una bolla è fuori limite tempo stabilito x reclami
014300030304     C                   eval      xx8 = wrkXX8
014400991025     C*
014500010723     C* Verifico la rottura i codice x file di input
014600010723     C                   if        (vacnfi <> depnfi   and
014700030304     C                              depnfi <> *blanks) or
014800030304     C                             sqlcod =  100
014900010723     C*
015000010726     C                   write     ttvacpt000
015100010726     C                   clear                   ttvacpt000
015200010723     C* Memorizzo il nuovo file di input x prossima rottura di codice
015300010723     C                   eval      depnfi = vacnfi
015400010723     C*---------------------------------
015500011203     C* VALORIZZO IL BUFFER DI OUTPUT
015600010723     C*---------------------------------
015700010726     C*
015800010726     C* Valorizzo campi costanti
015900010723     C                   eval      nomfil = vacnfi
016000010726     C                   eval      lancio = IDL
016100010723     C* Reperisco il raggruppamento serie e la ragione sociale
016200011203     C                   if        %scan('+':vacscx) = 0
016300040607     C                   EXSR      EXEBON
016400040607     C                   IF        EsitoBon <> '1'
016500010723     C                   movel     vacscx        SCLTOT
016600010723     C                   z-add     1             i                 3 0
016700010723     C     SCLNRS        lookup    RSRNRS(i)                              55
016800010723     C                   if        %found
016900010723     C                   movel     RSRRGR(i)     rgrnrs
017000021022     C                   eval      ragsoc = RSRDES(i) + %editc(vacNRS:'1')
017100010723     C                   endif
017200011203     C                   endif
017300040607     C                   ENDIF
017400010726     C* Conteggio segnacolli DOPPIO INPUT sulla totalità degli invii
017500010726     C                   if        vacxx5 <> *blanks
017600010726     C                   eval      dopitt = dopitt + 1
017700010726     C                   endif
017800021023     C* Conteggio i "persi" doppi su tutti gli invii
017900021023     C                   if        vacxx5 <> *blanks
018000021023     C* Doppi x consegna anomala 5
018100021023     C                   if        vaccca = '5'
018200021023     C                   eval      dopca5 = dopca5 + 1
018300021023     C                   endif
018400021023     C* Doppi x consegna anomala 7
018500021023     C                   if        vaccca = '7'
018600021023     C                   eval      dopca7 = dopca7 + 1
018700021023     C                   endif
018800021023     C                   endif
018900021023     C* X segnacolli NO DOPPIA SPED BARTOLINI
019000010723     C                   if        vacxx4 = *blanks
019100010723     C* Consegna anomala 5
019200010723     C                   if        vaccca = '5'
019300010723     C                   eval      cntca5 = cntca5 + 1
019400010726     C* ... di cui VEDI PACCO (x CA5)
019500010723     C                   if        %trim(vacrsd) = 'VEDI PACCO'
019600010723     C                   eval      vedca5 = vedca5 + 1
019700010723     C                   endif
019800010723     C                   endif
019900010723     C* Consegna anomala 7
020000010723     C                   if        vaccca = '7'
020100010723     C                   eval      cntca7 = cntca7 + 1
020200010726     C* ... di cui VEDI PACCO (x CA7)
020300010723     C                   if        %trim(vacrsd) = 'VEDI PACCO'
020400010723     C                   eval      vedca7 = vedca7 + 1
020500010723     C                   endif
020600010723     C                   endif
020700010723     C* VEDI PACCO (no CA5 e no CA7)
020800010723     C                   if        vaccca <> '5' and
020900010723     C                             vaccca <> '7' and
021000010723     C                             %trim(vacrsd) = 'VEDI PACCO'
021100010723     C                   eval      vedpac = vedpac + 1
021200010723     C                   endif
021300010723     C* Segnacolli dati a SDA
021400010723     C                   if        vaccca <> *blanks and
021500010723     C                             vaccca <> '5'     and
021600010723     C                             vaccca <> '7'
021700010723     C                   eval      cntsda = cntsda + 1
021800010723     C                   endif
021900010723     C* Segnacolli consegnati
022000010723     C                   if        vaccca = *blanks and
022100010723     C                             vacdcm > *zeros
022200010723     C                   eval      cntcon = cntcon + 1
022300010723     C                   endif
022400010723     C* Segnacolli non trovati
022500010723     C                   if        vacxx1 <> *blanks
022600010723     C                   eval      nontro = nontro + 1
022700010723     C                   endif
022800010723     C* Lettere di anomalia
022900010723     C                   if        vacnca > *zeros
023000010723     C                   eval      cntlea = cntlea + 1
023100010723     C                   endif
023200010723     C* Lettere di anomalia (no CA5 e no CA7)
023300010723     C                   if        vacnca  > *zeros and
023400010723     C                             vaccca <> '5'    and
023500010723     C                             vaccca <> '7'
023600010723     C                   eval      cntaca = cntaca + 1
023700010723     C                   endif
023800010723     C* Segnacolli non consegnati
023900010726     C                   if        vacdcm = *zeros and
024000010726     C                             vacxx1 = *blanks
024100010723     C                   eval      noncon = noncon + 1
024200010723     C                   endif
024300010723     C* Totale record effettivi x file di input
024400010723     C                   eval      totrec = totrec + 1
024500010726     C* Segnacolli non univoci in bolle BAR
024600010726     C                   if        vacxx3 <> *blanks
024700010726     C                   eval      dopseg = dopseg + 1
024800010726     C                   endif
024900010723     C*
025000010723     C                   else
025100010723     C* Segnacolli + che singoli in input
025200010723     C                   eval      dopinp = dopinp + 1
025300010723     C*
025400010723     C                   endif
025500021022     C*
025600021022     C* Effettuo i conteggi dei "trovati" in relazione ad anno/semestre
025700021022     C                   if        vacxx1 = *blanks
025800021022     C                   if        vacdsp >= 20000000 AND
025900021022     C                             vacdsp <  20000700
026000021022     C                   eval      A00S01 = A00S01 + 1
026100021022     C                   endif
026200021022     C                   if        vacdsp >= 20000700 AND
026300021022     C                             vacdsp <  20010000
026400021022     C                   eval      A00S02 = A00S02 + 1
026500021022     C                   endif
026600021022     C                   if        vacdsp >= 20010000 AND
026700021022     C                             vacdsp <  20010700
026800021022     C                   eval      A01S01 = A01S01 + 1
026900021022     C                   endif
027000021022     C                   if        vacdsp >= 20010700 AND
027100021022     C                             vacdsp <  20020000
027200021022     C                   eval      A01S02 = A01S02 + 1
027300021022     C                   endif
027400021022     C                   if        vacdsp >= 20020000 AND
027500021022     C                             vacdsp <  20020700
027600021022     C                   eval      A02S01 = A02S01 + 1
027700021022     C                   endif
027800021022     C                   if        vacdsp >= 20020700 AND
027900021022     C                             vacdsp <  20030000
028000021022     C                   eval      A02S02 = A02S02 + 1
028100021022     C                   endif
028200021022     C                   if        vacdsp >= 20030000 AND
028300021022     C                             vacdsp <  20030700
028400021022     C                   eval      A03S01 = A03S01 + 1
028500021022     C                   endif
028600021022     C                   if        vacdsp >= 20030700 AND
028700021022     C                             vacdsp <  20040000
028800021022     C                   eval      A03S02 = A03S02 + 1
028900021022     C                   endif
029000021022     C                   if        vacdsp >= 20040000 AND
029100021022     C                             vacdsp <  20040700
029200021022     C                   eval      A04S01 = A04S01 + 1
029300021022     C                   endif
029400021022     C                   if        vacdsp >= 20040700 AND
029500021022     C                             vacdsp <  20050000
029600021022     C                   eval      A04S02 = A04S02 + 1
029700021022     C                   endif
029800021022     C                   if        vacdsp >= 20050000 AND
029900021022     C                             vacdsp <  20050700
030000021022     C                   eval      A05S01 = A05S01 + 1
030100021022     C                   endif
030200021022     C                   if        vacdsp >= 20050700 AND
030300021022     C                             vacdsp <  20060000
030400021022     C                   eval      A05S02 = A05S02 + 1
030500021022     C                   endif
030600021022     C                   endif
030700010723     C*
030800010723     C                   else
030900010723     C*---------------------------------
031000030304     C* VALORIZZO IL BUFFER DI OUTPUT
031100010723     C*---------------------------------
031200010723     C* Memorizzo il nuovo file di input x prossima rottura di codice
031300010723     C                   eval      depnfi = vacnfi
031400010726     C*
031500010726     C* Valorizzo campi costanti
031600010723     C                   eval      nomfil = vacnfi
031700010726     C                   eval      lancio = IDL
031800010723     C* Reperisco il raggruppamento serie e la ragione sociale
031900011203     C                   if        %scan('+':vacscx) = 0
032000040607     C                   EXSR      EXEBON
032100040607     C                   IF        EsitoBon <> '1'
032200010723     C                   movel     vacscx        SCLTOT
032300010723     C                   z-add     1             i                 3 0
032400010723     C     SCLNRS        lookup    RSRNRS(i)                              55
032500010723     C                   if        %found
032600010723     C                   movel     RSRRGR(i)     rgrnrs
032700021022     C                   eval      ragsoc = RSRDES(i) + %editc(vacNRS:'1')
032800010723     C                   endif
032900011203     C                   endif
033000040607     C                   ENDIF
033100010726     C* Conteggio segnacolli DOPPIO INPUT sulla totalità degli invii
033200010726     C                   if        vacxx5 <> *blanks
033300010726     C                   eval      dopitt = dopitt + 1
033400021023     C                   endif
033500021023     C* Conteggio i "persi" doppi su tutti gli invii
033600021023     C                   if        vacxx5 <> *blanks
033700021023     C* Doppi x consegna anomala 5
033800021023     C                   if        vaccca = '5'
033900021023     C                   eval      dopca5 = dopca5 + 1
034000021023     C                   endif
034100021023     C* Doppi x consegna anomala 7
034200021023     C                   if        vaccca = '7'
034300021023     C                   eval      dopca7 = dopca7 + 1
034400021023     C                   endif
034500021023     C                   endif
034600021023     C* X segnacolli NO DOPPIA SPED BARTOLINI
034700010723     C                   if        vacxx4 = *blanks
034800010723     C* Consegna anomala 5
034900010723     C                   if        vaccca = '5'
035000010723     C                   eval      cntca5 = cntca5 + 1
035100010726     C* ... di cui VEDI PACCO (x CA5)
035200010723     C                   if        %trim(vacrsd) = 'VEDI PACCO'
035300010723     C                   eval      vedca5 = vedca5 + 1
035400010723     C                   endif
035500010723     C                   endif
035600010723     C* Consegna anomala 7
035700010723     C                   if        vaccca = '7'
035800010723     C                   eval      cntca7 = cntca7 + 1
035900010726     C* ... di cui VEDI PACCO (x CA7)
036000010723     C                   if        %trim(vacrsd) = 'VEDI PACCO'
036100010723     C                   eval      vedca7 = vedca7 + 1
036200010723     C                   endif
036300010723     C                   endif
036400010723     C* VEDI PACCO (no CA5 e no CA7)
036500010723     C                   if        vaccca <> '5' and
036600010723     C                             vaccca <> '7' and
036700010723     C                             %trim(vacrsd) = 'VEDI PACCO'
036800010723     C                   eval      vedpac = vedpac + 1
036900010723     C                   endif
037000010723     C* Segnacolli dati a SDA
037100010723     C                   if        vaccca <> *blanks and
037200010723     C                             vaccca <> '5'     and
037300010723     C                             vaccca <> '7'
037400010723     C                   eval      cntsda = cntsda + 1
037500010723     C                   endif
037600010723     C* Segnacolli consegnati
037700010723     C                   if        vaccca = *blanks and
037800010723     C                             vacdcm > *zeros
037900010723     C                   eval      cntcon = cntcon + 1
038000010723     C                   endif
038100010723     C* Segnacolli non trovati
038200010723     C                   if        vacxx1 <> *blanks
038300010723     C                   eval      nontro = nontro + 1
038400010723     C                   endif
038500010723     C* Lettere di anomalia
038600010723     C                   if        vacnca > *zeros
038700010723     C                   eval      cntlea = cntlea + 1
038800010723     C                   endif
038900010723     C* Lettere di anomalia (no CA5 e no CA7)
039000010723     C                   if        vacnca  > *zeros and
039100010723     C                             vaccca <> '5'    and
039200010723     C                             vaccca <> '7'
039300010723     C                   eval      cntaca = cntaca + 1
039400010723     C                   endif
039500010723     C* Segnacolli non consegnati
039600010726     C                   if        vacdcm = *zeros and
039700010726     C                             vacxx1 = *blanks
039800010723     C                   eval      noncon = noncon + 1
039900010723     C                   endif
040000010723     C* Totale record effettivi x file di input
040100010723     C                   eval      totrec = totrec + 1
040200010726     C* Segnacolli non univoci in bolle BAR
040300010726     C                   if        vacxx3 <> *blanks
040400010726     C                   eval      dopseg = dopseg + 1
040500010726     C                   endif
040600010723     C*
040700010723     C                   else
040800010723     C* Segnacolli + che singoli in input
040900010723     C                   eval      dopinp = dopinp + 1
041000010723     C*
041100010723     C                   endif
041200021022     C*
041300021022     C* Effettuo i conteggi dei "trovati" in relazione ad anno/semestre
041400021022     C                   if        vacxx1 = *blanks
041500021022     C                   if        vacdsp >= 20000000 AND
041600021022     C                             vacdsp <  20000700
041700021022     C                   eval      A00S01 = A00S01 + 1
041800021022     C                   endif
041900021022     C                   if        vacdsp >= 20000700 AND
042000021022     C                             vacdsp <  20010000
042100021022     C                   eval      A00S02 = A00S02 + 1
042200021022     C                   endif
042300021022     C                   if        vacdsp >= 20010000 AND
042400021022     C                             vacdsp <  20010700
042500021022     C                   eval      A01S01 = A01S01 + 1
042600021022     C                   endif
042700021022     C                   if        vacdsp >= 20010700 AND
042800021022     C                             vacdsp <  20020000
042900021022     C                   eval      A01S02 = A01S02 + 1
043000021022     C                   endif
043100021022     C                   if        vacdsp >= 20020000 AND
043200021022     C                             vacdsp <  20020700
043300021022     C                   eval      A02S01 = A02S01 + 1
043400021022     C                   endif
043500021022     C                   if        vacdsp >= 20020700 AND
043600021022     C                             vacdsp <  20030000
043700021022     C                   eval      A02S02 = A02S02 + 1
043800021022     C                   endif
043900021022     C                   if        vacdsp >= 20030000 AND
044000021022     C                             vacdsp <  20030700
044100021022     C                   eval      A03S01 = A03S01 + 1
044200021022     C                   endif
044300021022     C                   if        vacdsp >= 20030700 AND
044400021022     C                             vacdsp <  20040000
044500021022     C                   eval      A03S02 = A03S02 + 1
044600021022     C                   endif
044700021022     C                   if        vacdsp >= 20040000 AND
044800021022     C                             vacdsp <  20040700
044900021022     C                   eval      A04S01 = A04S01 + 1
045000021022     C                   endif
045100021022     C                   if        vacdsp >= 20040700 AND
045200021022     C                             vacdsp <  20050000
045300021022     C                   eval      A04S02 = A04S02 + 1
045400021022     C                   endif
045500021022     C                   if        vacdsp >= 20050000 AND
045600021022     C                             vacdsp <  20050700
045700021022     C                   eval      A05S01 = A05S01 + 1
045800021022     C                   endif
045900021022     C                   if        vacdsp >= 20050700 AND
046000021022     C                             vacdsp <  20060000
046100021022     C                   eval      A05S02 = A05S02 + 1
046200021022     C                   endif
046300021022     C                   endif
046400010723     C*
046500010723     C                   endif
046600010723     C*
046700000000     C                   ENDSR
046800000000     C*------------------------------------------------------------------------*
046900010726     C* CARTBL - CARICA I RAGGRUPPAMENTI DELLE SERIE CLIENTI POSTE
047000000000     C*------------------------------------------------------------------------*
047100000000     C     CARTBL        BEGSR
047200000918     C*--------------------
047300000918     C* RAGGRUPPAMENTI SERIE CLIENTI POSTE
047400000918     C*--------------------
047500000918     C                   Z-ADD     0             i
047600000918     C     'RSR'         SETLL     TNTBE01L
047700000918     C                   DO        *HIVAL
047800000918     C     'RSR'         READE     TNTBE01L                               70
047900000918     C   70              LEAVE
048000000918     C     TBEATB        IFEQ      ' '
048100000918     C     TBELIN        ANDEQ     *BLANKS
048200000918     C     TBESIF        ANDEQ     *BLANKS
048300000918     C                   MOVEL     TBEUNI        DRSR
048400000918     C                   ADD       1             i
048500000918     C                   MOVEL(p)  TBEKE1        RSRNRS(i)
048600001117     C                   EVAL      RSRDES(i) = §RSRDESC
048700001117     C                   EVAL      RSRRGR(i) = §RSRRGR
048800000918     C                   ENDIF
048900000918     C                   ENDDO
049000001117     C*
049100000000     C                   ENDSR
049200040607     C*------------------------------------------------------------------------*
049300040607     C     EXEBON        BEGSR
049400040607     C*
049500040607     C* Imposto i parametri x il *pgm d "bonifica"
049600040607     C                   EVAL      TxtInOut   = vacscx
049700040607     C                   EVAL      ElencoChar = '1234567890'
049800040607     C                   EVAL      TipoElenco = *blanks
049900040607     C                   EVAL      CharSost   = *blanks
050000040607     C                   EVAL      UpperCase  = *blanks
050100040607     C                   EVAL      ChkNull    = '1'
050200040607     C                   EVAL      CharNull   = *blanks
050300040607     C                   EVAL      EsitoBon   = *blanks
050400040607     C*
050500040607     C* Effettuo la chiamata al *pgm d "bonifica"
050600040607     C                   CALL      'XCHKCHAR'
050700040607     C                   PARM                    TxtInOut       2048
050800040607     C                   PARM                    ElencoChar      256
050900040607     C                   PARM                    TipoElenco        1
051000040607     C                   PARM                    CharSost          1
051100040607     C                   PARM                    UpperCase         1
051200040607     C                   PARM                    ChkNull           1
051300040607     C                   PARM                    CharNull          1
051400040607     C                   PARM                    EsitoBon          1
051500040607     C*
051600040607     C                   ENDSR
051700000000     C*------------------------------------------------------------------------*
051800000000     C* *INZSR - ROUTINE INIZIALE
051900000000     C*------------------------------------------------------------------------*
052000000000     C     *INZSR        BEGSR
052100010726     C*
052200010726     C* Ricevo in input il parametro indicante l'ID LANCIO
052300010726     C     *ENTRY        PLIST
052400010726     C                   PARM                    IDL               5
052500000000     C*
052600010726     C* Carica dati da tabelle
052700000000     C                   EXSR      CARTBL
052800000000     C*
052900000000     C                   ENDSR
