000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100719     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300001221     H*--------------------------------------------------------------*
000400001221     H*         - RITORNO DATI AL CLIENTE (ACCETTAZIONE BOLLE)
000500000000     H*--------------------------------------------------------------*
000600001218     FTABEL00F  IF   E           K DISK
000700100719     FTITAS38C  IF   E           K DISK
000800100719     F                                     rename(titas000:titas800)
000900100719     F                                     rename(titas010:titas810)
001000100719     F                                     rename(titasP00:titas8P0)
001100001218     FTITA430C  IF   E           K DISK
001200001218     FTITAA30C  IF   E           K DISK
001300001221     FTNCSB03L  IF   E           K DISK
001400040531     FFIAR531C  IF   E           K DISK
001500050825     FFNVAB00T  UF A E             DISK    USROPN
001600050825     FTIVGD00F  UF A E             DISK    USROPN
001700001218     D*--------------------
001800001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001900001218     D*--------------------
002000090407     D KUNI            S              7  0 DIM(1000) DESCEND
002100100312     D*----------------------------------------------------------
002200090217     D TBO             S              2    DIM(20)                              TIPI BOLLA recupero
002300001218     D*--------------------
002400001218     D* DS ESTERNE
002500001218     D*--------------------
002600100719     D TITAS00F      E DS
002700900517     D KPJBA         E DS
002800001221     D DS3Q          E DS
002900090217     D DSTB          E DS
003000000526     D DSBL4J        E DS
003100050825     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003200050909     D trul47ds      e ds
003300080213     D TIS781DS      E DS
003400080905     D TIS781DFLO    E DS
003500080115     D DVGDFLO       E DS
003600001218     D*--------------------
003700001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003800001218     D*--------------------
003900050825     D DSINPUT         DS
004000050825     D  DATADA                        8  0
004100050825     D  DATAA                         8  0
004200050825     D  CODCLI                        7  0
004300050825     D  FLGIMI                        1
004400050825     D  FLGUNI                        1
004500050825     D  FLGOPZ                        1
004600080115     D  CODCLIVAS                     7  0
004700100312     D  TIPFILE                       2
004800080213     D  FLGEXE                        1
004900080213     D  TIPCLI                        1
005000140702     D  FLGSICS                       1
005100010131     D*--------------------
005200010131     D* DS DI SCOMPOSIZIONE DATE
005300010131     D*--------------------
005400010131     D                 DS
005500010131     D  AAAA                   1      4  0
005600010131     D  MMGG                   5      8  0
005700010131     D  DATAWRK                1      8  0
005800030704     D*--------------------
005900030704     D* DS DI RIDEFINIZIONE FIAR5 TIPO RECORD "BAN"
006000030704     D*--------------------
006100030704     D DAR5BAN       E DS
006200151019     D DAR5FAT       E DS
006300030704     D*--------------------
006400030704     D* DS DI SCOMPOSIZIONE CAMPO VABANT
006500030704     D*--------------------
006600030704     D                 DS
006700030704     D vabTBA                  1      2  0
006800030704     D vabNBA                  3      4  0
006900030704     D vabTB2                  5      6  0
007000030704     D vabNB2                  7      8  0
007100050825     D ds_vabANT               1      9  0
007200010605     D*--------------------
007300010605     D* VARIABILI DI WRK
007400010605     D*--------------------
007500030704     D wrklire         S             10  0
007600030704     D kar5TRD         S                   Like(ar5TRD) Inz('BAN')
007700090217     D K               S              4  0
007800001218
007900010605
008000010605
008100920812     C*---------------------------------------------------------------*
008200001218     C* MAIN
008300001218     C*---------------------------------------------------------------*
008400100719     C*
008500100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008600100719     C
008700100719     C/EXEC SQL
008800100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008900100719     C/END-EXEC
009000100719     C
009100100719     C*
009200001218     C                   exsr      cartbl
009300001218     C                   exsr      chkpar
009400050909     C*
009500050909     C* Inizializzo variabili d wrk
009600050909     C                   movel     'N'           wProcedi          1
009700050909     C*
009800050909     C* Se richiesta scrittura sul file 00T => elaboro
009900050909     C                   if        *in25 = *on
010000050909     C                   open      fnvab00t
010100050909     C                   movel     'S'           wProcedi
010200050909     C                   endif
010300050909     C*
010400050909     C* Se richiesta scrittura sul file TIVGD00F => elaboro
010500050909     C* e come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'BT'
010600050909     C                   if        *in26 = *on
010700050909     C                   open      tivgd00f
010800050909     C                   clear                   trul47ds
010900050909     C                   eval      d47opz  = 'I'
011000120927     C                   eval      d47tip  = TIPFILE
011100050909     C                   eval      d47lck  = 'N'
011200050909     C                   eval      d47chkj = 'N'
011300050909     C                   eval      d47pgm  = 'TIS7VBR'
011400050909     C                   call      'TRUL47R'
011500050909     C                   parm                    trul47ds
011600050909     C*
011700050909     C* Se elaborazione consentita => proseguo
011800120928     C***                if        d47sts <> 'A'
011900050909     C                   movel     'S'           wProcedi
012000120928     C***                endif
012100050909     C                   endif
012200050909     C*
012300050909     C* Se ok a procedere => elaboro
012400050909     C                   if        wProcedi = 'S'
012500010131     C                   if        FLGIMI <> 'S'
012600001218     C                   exsr      procedi
012700010131     C                   else
012800010131     C                   exsr      procimi
012900010131     C                   endif
013000050909     C                   endif
013100050909     C*
013200050909     C* Chiudo il file di output
013300050909     C   25              close     fnvab00t
013400050909     C   26              close     tivgd00f
013500050909     C*
013600050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
013700050909     C                   clear                   trul47ds
013800050909     C                   eval      d47opz  = 'F'
013900120928     C                   eval      d47tip  = TIPFILE
014000050909     C                   call      'TRUL47R'
014100050909     C                   parm                    trul47ds
014200080213     C*
014300080213     C                   if        FLGOPZ = '2'
014400080213     C                   exsr      forzaDWN
014500080213     C                   endif
014600001218     C*
014700001218     C                   seton                                        LR
014800001218
014900001218
015000001218
015100001218
015200001218
015300001218     C*------------------------------------------------------------------------*
015400001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
015500001218     C*------------------------------------------------------------------------*
015600001218     C     CHKPAR        BEGSR
015700001218     C*
015800001218     C* Verifico le date
015900001218     C                   if        DATAA  = *zeros
016000001218     C                   movel     *all'9'       DATAA
016100001218     C                   endif
016200050825     C*
016300050825     C* Verifico la modalità d scrittura output richiesta:
016400050825     C*   1 = crea membro "RIC" in file FNVAB00T
016500050825     C*   2 = scrivi record in file TIVGD00F
016600050825     C                   setoff                                       2526
016700050825     C                   if        FLGOPZ = '1'
016800050825     C                   seton                                        25
016900050825     C                   endif
017000050825     C                   if        FLGOPZ = '2'
017100050825     C                   seton                                        26
017200050825     C                   endif
017300001218     C*
017400001218     C                   ENDSR
017500001218     C*------------------------------------------------------------------------*
017600001218
017700001218
017800001218
017900001218     C*------------------------------------------------------------------------*
018000001218     C* PROCEDI - Routine principale
018100001218     C*------------------------------------------------------------------------*
018200001218     C     PROCEDI       BEGSR
018300001218     C*
018400001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
018500001218     C                   z-add     1             I
018600090407     C                   sorta     KUNI
018700001218     C                   dow       KUNI(I) > *zeros
018800100719     C*
018900100719     C* Se richiesta elaborazione x codice cliente mittente...
019000100719     C                   select
019100100719     C                   when      TIPCLI = *blanks
019200100719     C                   exsr      EXECCM
019300100719     C*
019400100719     C* Se richiesta elaborazione x codice cliente fatturazione...
019500100719     C                   when      TIPCLI = 'K'
019600100719     C                   exsr      EXEKSC
019700100719     C*
019800100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019900100719     C                   when      TIPCLI = 'E'
020000100719     C                   exsr      EXECCM
020100100719     C                   exsr      EXEKSC
020200100719     C*
020300100719     C                   endsl
020400001218     C*
020500001218     C                   add       1             I
020600001218     C                   enddo
020700001218     C*
020800001218     C                   ENDSR
020900001218     C*------------------------------------------------------------------------*
021000100719
021100100719
021200100719
021300100719     C*------------------------------------------------------------------------*
021400100719     C* EXECCM   - Elaborazione x codice cliente mittente
021500100719     C*------------------------------------------------------------------------*
021600100719     C     EXECCM        BEGSR
021700100719     C*
021800100719     C                   movel     'C'           wChkCli           1
021900100719     C*
022000100719     C                   exsr      SQ_CCMDSP
022100100719     C*
022200100719     C                   ENDSR
022300100719     C*------------------------------------------------------------------------*
022400100719
022500100719
022600100719
022700100719     C*------------------------------------------------------------------------*
022800100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
022900100719     C*------------------------------------------------------------------------*
023000100719     C     EXEKSC        BEGSR
023100100719     C*
023200100719     C                   movel     'K'           wChkCli           1
023300100719     C*
023400100719     C                   exsr      SQ_KSCDSP
023500100719     C*
023600100719     C                   ENDSR
023700100719     C*------------------------------------------------------------------------*
023800100719
023900100719
024000100719
024100100719     C*------------------------------------------------------------------------*
024200100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
024300100719     C*------------------------------------------------------------------------*
024400100719     C     SQ_CCMDSP     BEGSR
024500100719     C*
024600100719     C                   z-add     KUNI(I)       wParCCM           7 0
024700100719     C*
024800100719     C/EXEC SQL
024900100719     C+ declare C_CCMDSP cursor for
025000100719     C+ select * from titas00f
025100100719     C+ where tasccm = :wParCCM and
025200100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025300100719     C+ union
025400100719     C+ select * from titas10f
025500100719     C+ where tasccm = :wParCCM and
025600100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025700100719     C+ union
025800100719     C+ select * from titasP0f
025900100719     C+ where tasccm = :wParCCM and
026000100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026100100719     C+ for read only
026200100719     C/END-EXEC
026300100719     C
026400100719     C/EXEC SQL
026500100719     C+ open C_CCMDSP
026600100719     C/END-EXEC
026700100719     C
026800100719     C/EXEC SQL
026900100719     C+ Fetch C_CCMDSP into :TITAS00F
027000100719     C/END-EXEC
027100100719     C*
027200100719     C                   dow       sqlcod = *zeros
027300100719     C                   exsr      verifica
027400100719     C                   if        CHKREC = 'S'
027500100719     C                   exsr      scriviVAB
027600100719     C                   endif
027700100719     C
027800100719     C/EXEC SQL
027900100719     C+ Fetch C_CCMDSP into :TITAS00F
028000100719     C/END-EXEC
028100100719     C*
028200100719     C                   enddo
028300100719     C*
028400100719     C/EXEC SQL
028500100719     C+ close C_CCMDSP
028600100719     C/END-EXEC
028700100719     C*
028800100719     C*
028900100719     C                   ENDSR
029000100719     C*------------------------------------------------------------------------*
029100100719
029200100719
029300100719
029400100719     C*------------------------------------------------------------------------*
029500100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029600100719     C*------------------------------------------------------------------------*
029700100719     C     SQ_KSCDSP     BEGSR
029800100719     C*
029900100719     C                   z-add     KUNI(I)       wParKSC           7 0
030000100719     C*
030100100719     C/EXEC SQL
030200100719     C+ declare C_KSCDSP cursor for
030300100719     C+ select * from titas00f
030400100719     C+ where tasksc = :wParKSC and
030500100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030600100719     C+ union
030700100719     C+ select * from titas10f
030800100719     C+ where tasksc = :wParKSC and
030900100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031000100719     C+ union
031100100719     C+ select * from titasP0f
031200100719     C+ where tasksc = :wParKSC and
031300100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031400100719     C+ for read only
031500100719     C/END-EXEC
031600100719     C
031700100719     C/EXEC SQL
031800100719     C+ open C_KSCDSP
031900100719     C/END-EXEC
032000100719     C
032100100719     C/EXEC SQL
032200100719     C+ Fetch C_KSCDSP into :TITAS00F
032300100719     C/END-EXEC
032400100719     C*
032500100719     C                   dow       sqlcod = *zeros
032600100719     C                   exsr      verifica
032700100719     C                   if        CHKREC = 'S'
032800100719     C                   exsr      scriviVAB
032900100719     C                   endif
033000100719     C
033100100719     C/EXEC SQL
033200100719     C+ Fetch C_KSCDSP into :TITAS00F
033300100719     C/END-EXEC
033400100719     C*
033500100719     C                   enddo
033600100719     C*
033700100719     C/EXEC SQL
033800100719     C+ close C_KSCDSP
033900100719     C/END-EXEC
034000100719     C*
034100100719     C*
034200100719     C                   ENDSR
034300100719     C*------------------------------------------------------------------------*
034400100719
034500001218
034600001218
034700010131     C*------------------------------------------------------------------------*
034800010131     C* PROCIMI - Routine principale per richiesta lancio IMI
034900010131     C*------------------------------------------------------------------------*
035000081006     C     PROCIMI       BEGSR
035100010131     C*
035200010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
035300010131     C                   z-add     DATADA        DATAWRK
035400010131     C                   z-add     AAAA          depAAS
035500010131     C                   z-add     MMGG          depMGS
035600010131     C*
035700010131     C     K38C          setll     titas38c
035800010131     C                   read      titas38c
035900010131     C*
036000010131     C                   dow       not %eof(titas38c)
036100010131     C* Verifico di non essere fuori range date
036200010131     C                   z-add     tasaas        AAAA
036300010131     C                   z-add     tasmgs        MMGG
036400010131     C                   if        DATAWRK > DATAA
036500010131     C                   leave
036600010131     C                   else
036700010131     C                   exsr      verifica
036800010131     C                   if        CHKREC = 'S'
036900010131     C                   exsr      scriviVAB
037000010131     C                   endif
037100010131     C                   endif
037200010131     C                   read      titas38c
037300010131     C                   enddo
037400010131     C*
037500010131     C                   ENDSR
037600010131     C*------------------------------------------------------------------------*
037700010131
037800010131
037900001218
038000001218     C*------------------------------------------------------------------------*
038100001218     C* VERIFICA - Routine di verifica validità record corrente
038200001218     C*------------------------------------------------------------------------*
038300001218     C     VERIFICA      BEGSR
038400001218     C*
038500001218     C                   movel     'S'           CHKREC            1
038600001218     C*
038700001218     C* Verifico le date
038800001218     C                   if        CHKREC = 'S'
038900001218     C                   movel     tasaas        wrkdata           8 0
039000001218     C                   move      tasmgs        wrkdata
039100001218     C                   if        wrkdata < DATADA or
039200001218     C                             wrkdata > DATAA
039300001218     C                   movel     'N'           CHKREC
039400001218     C                   endif
039500001218     C                   endif
039600001218     C*
039700010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
039800001218     C                   if        CHKREC = 'S'
039900010131     C                   if        FLGIMI <> 'S'
040000010301     C                   if        tasksc <> KUNI(I)
040100010301     C                   movel     'N'           CHKREC
040200001218     C                   endif
040300010131     C                   endif
040400001218     C                   endif
040500010131     C*
040600010131     C* Verifico esistenza IMI qualora richiesto nel lancio
040700010131     C                   if        CHKREC = 'S'
040800010131     C                   if        FLGIMI = 'S'
040900010131     C                   if        tasftc = 'F' or tastc2 = 'F'
041000010131     C                   else
041100010131     C                   movel     'N'           CHKREC
041200010131     C                   endif
041300010131     C                   endif
041400010131     C                   endif
041500100719     C*
041600100719     C* Verifico il codice cliente mittente
041700100719     C                   if        CHKREC = 'S'
041800100719     C                   if        wChkCli  = 'C'      AND
041900100719     C                             tasccm  <> KUNI(I)
042000100719     C                   movel     'N'           CHKREC
042100100719     C                   endif
042200100719     C                   endif
042300100719     C*
042400100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
042500100719     C                   if        CHKREC = 'S'
042600100719     C                   if        wChkCli  = 'K'      AND
042700100719     C                             tasksc  <> KUNI(I)
042800100719     C                   movel     'N'           CHKREC
042900100719     C                   endif
043000100719     C                   endif
043100100719     C*
043200100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
043300100719     C                   if        CHKREC = 'S'
043400100719     C                   if        TIPCLI  = 'E'       AND
043500100719     C                             wChkCli = 'K'       AND
043600100719     C                             tasksc  = tasccm
043700100719     C                   movel     'N'           CHKREC
043800100719     C                   endif
043900100719     C                   endif
044000140702     C*
044100090217     C* Verifico il tipo bolla che non deve essere di reupero
044200090217     C                   if        CHKREC = 'S'
044300090217     C                   if        %lookup(tastbl:tbo)>0
044400090217     C                   movel     'N'           CHKREC
044500090217     C                   endif
044600090217     C                   endif
044700001218     C*
044800001218     C                   ENDSR
044900001218     C*------------------------------------------------------------------------*
045000001218
045100001218
045200001218
045300001218     C*------------------------------------------------------------------------*
045400001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
045500001218     C*------------------------------------------------------------------------*
045600001221     C     SCRIVIVAB     BEGSR
045700001218     C*
045800001227     C                   clear                   fnvab000
045900010131     C                   if        FLGIMI = 'S'
046000010131     C                   seton                                        55
046100010131     C                   else
046200010131     C                   setoff                                       55
046300010131     C                   endif
046400151019     C*
046500151019     C* Eseguo eventuali "forzature"
046600151019     C                   EXSR      RTVAR5FAT
046700151019     C*
046800001221     C                   movel     tascbo        vabcbo
046900001221     C                   movel     tasrsd        vabrsd
047000001221     C                   movel     tasind        vabind
047100001221     C                   movel     tascad        vabcad
047200001221     C                   movel     taslod        vablod
047300001221     C                   movel     tasprd        vabprd
047400001221     C                   movel     tasnzd        vabnzd
047500001221     C                   movel     tasgc1        vabgc1
047600001221     C                   movel     tasgc2        vabgc2
047700001221     C                   movel     tastsp        vabtsp
047800001221     C                   movel     tasnas        vabnas
047900001221     C                   movel     tasctm        vabctm
048000001221     C                   movel     tasffd        vabffd
048100001221     C                   movel     tastcr        vabtcr
048200001221     C                   movel     tascts        vabcts
048300001221     C                   movel     tasftm        vabftm
048400001221     C                   movel     tasgma        vabgma
048500001221     C                   movel     tasgga        vabgga
048600001221     C                   movel     tasgva        vabgva
048700010111     C                   move      ' '           vabgva
048800100927     C                   movel     tasftc        vabtc1
048900100927     C                   movel     tastc2        vabtc2
049000001221     C                   if        tasccm > 0
049100001221     C                   z-add     tasccm        vabccm
049200001221     C                   else
049300001221     C                   z-add     tasksc        vabccm
049400001221     C                   end
049500001221     C                   z-add     taslnp        vablnp
049600001221     C                   z-add     tasaas        vabaas
049700001221     C                   z-add     tasmgs        vabmgs
049800001221     C                   z-add     tasnrs        vabnrs
049900001221     C                   z-add     tasnsp        vabnsp
050000001221     C                   z-add     taslna        vablna
050100001221     C                   z-add     tasctr        vabctr
050200001221     C                   z-add     tasncl        vabncl
050300001221     C                   z-add     taspkf        vabpkb
050400001221     C                   z-add     tasvlf        vabvlb
050500001221     C                   z-add     tasqft        vabqft
050600001221     C                   z-add     tasrmn        vabrmn
050700001221     C                   z-add     tasncd        vabncd
050800001221     C                   z-add     tasnca        vabnca
050900001221     C                   z-add     tasznc        vabznc
051000010108     C                   z-add     tasdbr        vabdcr
051100001221     C* Effettuo considerazioni particolare per bolla "POSTE"
051200001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
051300001227     C                   exsr      REPPTIAS
051400001227     C                   else
051500001227     C                   movel     tasIAS        vabIAS
051600001227     C                   movel     tasVAS        vabVAS
051700001227     C                   movel     tasXCO        vabXCO
051800001227     C                   endif
051900001227     C* Considerazioni per bolle "autogenerate"
052000001227     C                   if        tasSOP = '&' and
052100001227     C                             vabSCL = *blanks
052200001227     C                   movel     'A'           vabSCL
052300001227     C                   endif
052400001227     C*
052500001227     C                   exsr      REPCSB
052600010131     C                   exsr      REPRMA
052700081006     C                   exsr      REPANT
052800081006     C                   exsr      REPDEST
052900010605     C*
053000010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
053100010605     C                   exsr      REPIMP
053200001218     C*
053300010131     C                   if        *in55 = *off
053400050825     C   25              WRITE     FNVAB000
053500050825     C   26              exsr      WRITIVGD
053600010131     C                   endif
053700001218     C*
053800001218     C                   ENDSR
053900001221     C*------------------------------------------------------------------------*
054000050825
054100050825
054200050825
054300050825
054400050825     C*------------------------------------------------------------------------*
054500050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
054600050825     C*------------------------------------------------------------------------*
054700050825     C     WRITIVGD      BEGSR
054800050825     C*
054900050825     C                   clear                   tivgd000
055000080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
055100100312     C                   eval      vgdTIP = TIPFILE
055200050825     C                   movel     *all'0'       vgdKSU
055300080115     C                   move      CODCLIVAS     vgdKSU
055400050825     C                   eval      vgdTSC = 'WW'
055500050825     C                   eval      vgdDAT = datcor
055600050825     C                   eval      vgdPGM = 'TIS7VBR'
055700080213     C*
055800080213     C                   clear                   DVGDFLO
055900080213     C                   movel     'P'           §VGDFELA
056000080213     C                   movel     DVGDFLO       vgdflo
056100080115     C*
056200050825     C                   write     tivgd000
056300050825     C*
056400050825     C                   ENDSR
056500050825     C*------------------------------------------------------------------------*
056600080213
056700080213
056800080213
056900080213     C*------------------------------------------------------------------------*
057000080213     C* FORZADWN - Routine di forzatura elaborazione immediata download
057100080213     C*------------------------------------------------------------------------*
057200080213     C     FORZADWN      BEGSR
057300080213     C*
057400080213     C                   clear                   tis781ds
057500080213     C                   eval      §781tip = vgdTIP
057600080213     C                   eval      §781ksu = vgdKSU
057700080213     C                   eval      §781tsc = vgdTSC
057800080213     C                   eval      §781dat = vgdDAT
057900080213     C                   eval      §781pgm = vgdPGM
058000080905     C*
058100080905     C                   clear                   TIS781DFLO
058200080905     C                   movel     'P'           §781floela
058300080905     C                   movel     TIS781DFLO    §781flo
058400080213     C*
058500080213     C* Finita l'elaborazione chiamo pgm
058600080213     C                   CALL      'TIS781C1'
058700080213     C                   parm      *blanks       esito             1
058800080213     C                   parm                    tis781ds
058900080213     C                   parm      *blanks       vgdPRG           10
059000080213     C*
059100080213     C                   ENDSR
059200080213     C*------------------------------------------------------------------------*
059300001218
059400001221
059500001221
059600001218
059700001218     C*------------------------------------------------------------------------*
059800001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
059900010131     C*          - e dei pacchi ingombranti non dichiarati
060000001218     C*------------------------------------------------------------------------*
060100001218     C     REPPTIAS      BEGSR
060200001218     C*
060300060215     D* Prendo importo da assicurare da TITA4 t.r.=J
060400001218     C                   movel     'J'           KTRC
060500001218     C     KTA4          chain     tita430c
060600001218     C                   if        %found(tita430c)
060700001218     C                   movel     ta4NOT        DSBL4J
060800001221     C                   z-add     §B4IAS        vabIAS
060900001221     C                   movel     §B4VAS        vabVAS
061000001221     C                   movel     §B4XCO        vabXCO
061100001227     C                   movel     §B4BAU        vabSCL
061200010131     C                   if        §B4BAI = 'S'
061300010131     C                   movel     §B4BAI        vabFTM
061400010131     C                   setoff                                       55
061500010131     C                   endif
061600001218     C                   endif
061700001218     C*
061800001218     C                   ENDSR
061900001218     C*------------------------------------------------------------------------*
062000001218
062100001218
062200001218
062300001218
062400001218     C*------------------------------------------------------------------------*
062500001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
062600001218     C*------------------------------------------------------------------------*
062700001218     C     REPRMA        BEGSR
062800001218     C*
062900060215     D* Prendo importo da assicurare da TITA4 t.r.=A
063000001218     C                   movel     'A'           KTRC
063100001218     C     KTA4          chain     tita430c
063200001218     C                   if        %found(tita430c)
063300001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
063400001218     C                   endif
063500001218     C*
063600001218     C                   ENDSR
063700001218     C*------------------------------------------------------------------------*
063800001218
063900001218
064000001218
064100001218
064200001218     C*------------------------------------------------------------------------*
064300001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
064400001218     C*           ... e ai dati del mittente originale
064500001218     C*------------------------------------------------------------------------*
064600001218     C     REPDEST       BEGSR
064700001218     C*
064800001218     D* Prendo il tipo anagrafica = D
064900001218     C                   movel     'D'           KTRC
065000010131     C     KTAA          chain     titaa30c
065100001218     C                   if        %found(titaa30c)
065200001221     C                   movel     taaRSC        vabRD2
065300001218     C                   endif
065400001218     C*
065500001221     D* Prendo il tipo anagrafica = O
065600001218     C                   movel     'O'           KTRC
065700001218     C     KTAA          chain     titaa30c
065800001218     C                   if        %found(titaa30c)
065900001221     C                   movel     taaRSC        vabRMO
066000120326     C***                movel     taaCAP        vabCMO
066100120326     C***                movel     taaNAZ        vabNMO
066200001218     C                   endif
066300001222     C                   if        vabNMO = *blanks
066400001222     C                   move      '.'           vabNMO
066500001222     C                   endif
066600001218     C*
066700001218     C                   ENDSR
066800001218     C*------------------------------------------------------------------------*
066900001218
067000001218
067100001218
067200001218
067300001218     C*------------------------------------------------------------------------*
067400001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
067500001218     C*------------------------------------------------------------------------*
067600001218     C     REPCSB        BEGSR
067700001218     C*
067800001219     C     KCSB          chain     tncsb03l
067900001219     C                   if        %found(tncsb03l)
068000001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
068100001218     C                   else
068200001221     C                   movel     csbTPA        vabTIC
068300001221     C                   move      csbTPI        vabTIC
068400001221     C                   z-add     csbCAS        vabCAS
068500001221     C                   movel     csbVCA        vabVCA
068600001221     C                   movel     csbGCA        vabGCA
068700001218     C                   endif
068800001218     C                   endif
068900001218     C*
069000001218     C                   ENDSR
069100001218     C*------------------------------------------------------------------------*
069200001218
069300010605
069400010605
069500010605
069600010605     C*------------------------------------------------------------------------*
069700010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
069800010605     C*------------------------------------------------------------------------*
069900010605     C     REPIMP        BEGSR
070000010605     C*
070100010605     C* Come prima cosa verifico se trattasi di bolla POSTE
070200010605     C                   if        vabtsp = 'P'
070300010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
070400010605     C                   if        vabvas = 'EUR'
070500010605     C                   eval      vabvmd = vabias
070600010605     C                   eval(h)   wrklire = vabvmd * 1936,27
070700010605     C                   eval      vabias = wrklire
070800010605     C                   eval      vabvas = *blanks
070900010605     C                   endif
071000010605     C*
071100010605     C                   if        vabvca = 'EUR'
071200010605     C                   eval      vabqft = vabcas
071300010605     C                   eval(h)   wrklire = vabqft * 1936,27
071400010605     C                   eval      vabcas = wrklire
071500010605     C                   eval      vabvca = *blanks
071600010605     C                   endif
071700010605     C*
071800010605     C                   eval      vabxco = *blanks
071900010605     C                   endif
072000010605     C*
072100010605     C                   ENDSR
072200030704
072300030704
072400030704
072500030704
072600030704     C*------------------------------------------------------------------------*
072700030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
072800030704     C*------------------------------------------------------------------------*
072900030704     C     REPANT        BEGSR
073000030704     C*
073100050825     C                   clear                   ds_vabANT
073200030704     C                   if        %subst(tasGVA:1:1) = 'B'
073300151019     C                   eval      kar5TRD = 'BAN'
073400040531     C     KFIAR5        chain     fiar531c
073500040531     C                   if        %found(fiar531c)
073600030704     C                   movel     ar5UNI        DAR5BAN
073700030704     C                   movel     §ar5TBA       vabTBA
073800030704     C                   movel     §ar5NBA       vabNBA
073900030704     C                   movel     §ar5TB2       vabTB2
074000030704     C                   movel     §ar5NB2       vabNB2
074100030704     C                   endif
074200030704     C                   endif
074300050825     C                   eval      vabANT = ds_vabANT
074400030704     C*
074500030704     C                   ENDSR
074600030704     C*------------------------------------------------------------------------*
074700010605
074800010605
074900010605
075000001218
075100001218
075200001218     C*------------------------------------------------------------------------*
075300001218     C* CARTBL - Routine di caricamento dati tabellati
075400001218     C*------------------------------------------------------------------------*
075500001218     C     CARTBL        BEGSR
075600001218     C*
075700001221     C                   Z-ADD     0             I                 4 0
075800050825     C                   IF        FLGUNI = 'S'
075900010301     C                   ADD       1             I
076000050825     C                   MOVEL     CODCLI        KUNI(I)
076100010301     C                   ELSE
076200001221     C                   MOVEL     '3Q'          COD
076300001218     C     KTAB          CHAIN     TABEL                              31
076400001218     C     *IN31         DOWEQ     '0'
076500090518     C                   IF        TBLFLG = ' '
076600090518     C***                IF        TBLFLG = ' ' AND
076700090518     C***                          %subst(TBLKEY:8:1) = ' '
076800001222     C                   MOVEL     TBLUNI        DS3Q
076900050825     C     §3QCKS        IFEQ      CODCLI
077000001221     C                   ADD       1             I
077100001221     C                   MOVEL     TBLKEY        KUNI(I)
077200001221     C                   ENDIF
077300001218     C                   ENDIF
077400001218     C     KTAB          READE     TABEL                                  31
077500001218     C                   ENDDO
077600010301     C                   ENDIF
077700120326     c* carico schiera tipi bolla nn validi ai fini della spedizione
077800090217     c                   clear                   k
077900090217     c                   movel     'TB'          cod
078000090217     c     ktab          setll     tabel00f
078100090217     c     ktab          reade     tabel00f
078200090217     c                   dow       not %eof(tabel00f)
078300090217     c                   if        tblflg<>'*'
078400090217     C                   movel     tbluni        dstb
078500140702     C                   IF        §tbrbl='R'
078600090217     C                   ADD       1             K
078700090217     C                   MOVEL     TBLKEY        TBO(K)
078800090217     c                   endif
078900140702     C                   IF        §tbrbl='C' and FLGSICS <> 'S'
079000140702     C                   ADD       1             K
079100140702     C                   MOVEL     TBLKEY        TBO(K)
079200140702     c                   endif
079300090217     c                   endif
079400090217     c     ktab          reade     tabel00f
079500090217     c                   enddo
079600090217     c
079700001218     C*
079800001218     C                   ENDSR
079900001221     C*------------------------------------------------------------------------*
080000151019
080100151019
080200151019
080300151019     C     RTVAR5FAT     BEGSR
080400151019     C*
080500151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
080600151019     C                   SETOFF                                       7071
080700151019     C                   EVAL      kar5TRD = 'FAT'
080800151019     C                   CLEAR                   DAR5FAT
080900151019     C     KFIAR5        CHAIN     FIAR531C
081000151019     C                   IF        %found(FIAR531C)
081100151019     C                   MOVEL     AR5UNI        DAR5FAT
081200151019     C                   IF        §AR5PKTAS > *zeros
081300151019     C                   SETON                                        70
081400151019     C                   EVAL      tasPKF = §AR5PKTAS
081500151019     C                   ENDIF
081600151019     C                   IF        §AR5VLTAS > *zeros
081700151019     C                   SETON                                        71
081800151019     C***                EVAL      tasVLF = §AR5VLTAS
081900151019     C                   ENDIF
082000151019     C                   ENDIF
082100151019     C*
082200151019     C                   ENDSR
082300001218
082400001218
082500001218
082600001218     C*------------------------------------------------------------------------*
082700001218     C* *INZSR - ROUTINE INIZIALE
082800001218     C*------------------------------------------------------------------------*
082900001218     C     *INZSR        BEGSR
083000001218     C*
083100001218     C     *ENTRY        PLIST
083200140702     C                   PARM                    PARAM            38
083300001218     C*
083400001218     C                   MOVEL     PARAM         DSINPUT
083500001218     C                   Z-ADD     1             CODUT
083600001218     C*
083700001218     C* Definizioni chiavi
083800010131     C     K38C          KLIST
083900010131     C                   KFLD                    depAAS            4 0
084000010131     C                   KFLD                    depMGS            4 0
084100010131     C*
084200001218     C     KTA4          KLIST
084300001218     C                   KFLD                    tasAAS
084400001218     C                   KFLD                    tasLNP
084500001218     C                   KFLD                    tasNRS
084600001218     C                   KFLD                    tasNSP
084700001218     C                   KFLD                    KTRC              1
084800001218     C*
084900001218     C     KTAA          KLIST
085000001218     C                   KFLD                    tasAAS
085100001218     C                   KFLD                    tasLNP
085200001218     C                   KFLD                    tasNRS
085300001218     C                   KFLD                    tasNSP
085400001218     C                   KFLD                    KTRC
085500001218     C*
085600001218     C     KCSB          KLIST
085700001218     C                   KFLD                    tasAAS
085800001218     C                   KFLD                    tasLNP
085900001218     C                   KFLD                    tasNRS
086000001218     C                   KFLD                    tasNSP
086100001218     C*
086200001218     C     KTAB          KLIST
086300001218     C                   KFLD                    CODUT             1 0
086400001218     C                   KFLD                    COD               2
086500030704     C*
086600030704     C     KFIAR5        KLIST
086700030704     C                   KFLD                    tasAAS
086800030704     C                   KFLD                    tasLNP
086900030704     C                   KFLD                    tasNRS
087000030704     C                   KFLD                    tasNSP
087100030704     C                   KFLD                    kar5TRD
087200001218     C*
087300001218     C* Determino la data corrente
087400100312     C                   Z-ADD     *zeros        DATCOR            8 0
087500100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
087600001218     C*
087700001218     C                   ENDSR
