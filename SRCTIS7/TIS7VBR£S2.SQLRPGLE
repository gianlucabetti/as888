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
006200030704     D*--------------------
006300030704     D* DS DI SCOMPOSIZIONE CAMPO VABANT
006400030704     D*--------------------
006500030704     D                 DS
006600030704     D vabTBA                  1      2  0
006700030704     D vabNBA                  3      4  0
006800030704     D vabTB2                  5      6  0
006900030704     D vabNB2                  7      8  0
007000050825     D ds_vabANT               1      9  0
007100010605     D*--------------------
007200010605     D* VARIABILI DI WRK
007300010605     D*--------------------
007400030704     D wrklire         S             10  0
007500030704     D kar5TRD         S                   Like(ar5TRD) Inz('BAN')
007600090217     D K               S              4  0
007700001218
007800010605
007900010605
008000920812     C*---------------------------------------------------------------*
008100001218     C* MAIN
008200001218     C*---------------------------------------------------------------*
008300100719     C*
008400100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008500100719     C
008600100719     C/EXEC SQL
008700100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008800100719     C/END-EXEC
008900100719     C
009000100719     C*
009100001218     C                   exsr      cartbl
009200001218     C                   exsr      chkpar
009300050909     C*
009400050909     C* Inizializzo variabili d wrk
009500050909     C                   movel     'N'           wProcedi          1
009600050909     C*
009700050909     C* Se richiesta scrittura sul file 00T => elaboro
009800050909     C                   if        *in25 = *on
009900050909     C                   open      fnvab00t
010000050909     C                   movel     'S'           wProcedi
010100050909     C                   endif
010200050909     C*
010300050909     C* Se richiesta scrittura sul file TIVGD00F => elaboro
010400050909     C* e come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'BT'
010500050909     C                   if        *in26 = *on
010600050909     C                   open      tivgd00f
010700050909     C                   clear                   trul47ds
010800050909     C                   eval      d47opz  = 'I'
010900120927     C                   eval      d47tip  = TIPFILE
011000050909     C                   eval      d47lck  = 'N'
011100050909     C                   eval      d47chkj = 'N'
011200050909     C                   eval      d47pgm  = 'TIS7VBR'
011300050909     C                   call      'TRUL47R'
011400050909     C                   parm                    trul47ds
011500050909     C*
011600050909     C* Se elaborazione consentita => proseguo
011700120928     C***                if        d47sts <> 'A'
011800050909     C                   movel     'S'           wProcedi
011900120928     C***                endif
012000050909     C                   endif
012100050909     C*
012200050909     C* Se ok a procedere => elaboro
012300050909     C                   if        wProcedi = 'S'
012400010131     C                   if        FLGIMI <> 'S'
012500001218     C                   exsr      procedi
012600010131     C                   else
012700010131     C                   exsr      procimi
012800010131     C                   endif
012900050909     C                   endif
013000050909     C*
013100050909     C* Chiudo il file di output
013200050909     C   25              close     fnvab00t
013300050909     C   26              close     tivgd00f
013400050909     C*
013500050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
013600050909     C                   clear                   trul47ds
013700050909     C                   eval      d47opz  = 'F'
013800120928     C                   eval      d47tip  = TIPFILE
013900050909     C                   call      'TRUL47R'
014000050909     C                   parm                    trul47ds
014100080213     C*
014200080213     C                   if        FLGOPZ = '2'
014300080213     C                   exsr      forzaDWN
014400080213     C                   endif
014500001218     C*
014600001218     C                   seton                                        LR
014700001218
014800001218
014900001218
015000001218
015100001218
015200001218     C*------------------------------------------------------------------------*
015300001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
015400001218     C*------------------------------------------------------------------------*
015500001218     C     CHKPAR        BEGSR
015600001218     C*
015700001218     C* Verifico le date
015800001218     C                   if        DATAA  = *zeros
015900001218     C                   movel     *all'9'       DATAA
016000001218     C                   endif
016100050825     C*
016200050825     C* Verifico la modalità d scrittura output richiesta:
016300050825     C*   1 = crea membro "RIC" in file FNVAB00T
016400050825     C*   2 = scrivi record in file TIVGD00F
016500050825     C                   setoff                                       2526
016600050825     C                   if        FLGOPZ = '1'
016700050825     C                   seton                                        25
016800050825     C                   endif
016900050825     C                   if        FLGOPZ = '2'
017000050825     C                   seton                                        26
017100050825     C                   endif
017200001218     C*
017300001218     C                   ENDSR
017400001218     C*------------------------------------------------------------------------*
017500001218
017600001218
017700001218
017800001218     C*------------------------------------------------------------------------*
017900001218     C* PROCEDI - Routine principale
018000001218     C*------------------------------------------------------------------------*
018100001218     C     PROCEDI       BEGSR
018200001218     C*
018300001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
018400001218     C                   z-add     1             I
018500090407     C                   sorta     KUNI
018600001218     C                   dow       KUNI(I) > *zeros
018700100719     C*
018800100719     C* Se richiesta elaborazione x codice cliente mittente...
018900100719     C                   select
019000100719     C                   when      TIPCLI = *blanks
019100100719     C                   exsr      EXECCM
019200100719     C*
019300100719     C* Se richiesta elaborazione x codice cliente fatturazione...
019400100719     C                   when      TIPCLI = 'K'
019500100719     C                   exsr      EXEKSC
019600100719     C*
019700100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
019800100719     C                   when      TIPCLI = 'E'
019900100719     C                   exsr      EXECCM
020000100719     C                   exsr      EXEKSC
020100100719     C*
020200100719     C                   endsl
020300001218     C*
020400001218     C                   add       1             I
020500001218     C                   enddo
020600001218     C*
020700001218     C                   ENDSR
020800001218     C*------------------------------------------------------------------------*
020900100719
021000100719
021100100719
021200100719     C*------------------------------------------------------------------------*
021300100719     C* EXECCM   - Elaborazione x codice cliente mittente
021400100719     C*------------------------------------------------------------------------*
021500100719     C     EXECCM        BEGSR
021600100719     C*
021700100719     C                   movel     'C'           wChkCli           1
021800100719     C*
021900100719     C                   exsr      SQ_CCMDSP
022000100719     C*
022100100719     C                   ENDSR
022200100719     C*------------------------------------------------------------------------*
022300100719
022400100719
022500100719
022600100719     C*------------------------------------------------------------------------*
022700100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
022800100719     C*------------------------------------------------------------------------*
022900100719     C     EXEKSC        BEGSR
023000100719     C*
023100100719     C                   movel     'K'           wChkCli           1
023200100719     C*
023300100719     C                   exsr      SQ_KSCDSP
023400100719     C*
023500100719     C                   ENDSR
023600100719     C*------------------------------------------------------------------------*
023700100719
023800100719
023900100719
024000100719     C*------------------------------------------------------------------------*
024100100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
024200100719     C*------------------------------------------------------------------------*
024300100719     C     SQ_CCMDSP     BEGSR
024400100719     C*
024500100719     C                   z-add     KUNI(I)       wParCCM           7 0
024600100719     C*
024700100719     C/EXEC SQL
024800100719     C+ declare C_CCMDSP cursor for
024900100719     C+ select * from titas00f
025000100719     C+ where tasccm = :wParCCM and
025100100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025200100719     C+ union
025300100719     C+ select * from titas10f
025400100719     C+ where tasccm = :wParCCM and
025500100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
025600100719     C+ union
025700100719     C+ select * from titasP0f
025800100719     C+ where tasccm = :wParCCM and
025900100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026000100719     C+ for read only
026100100719     C/END-EXEC
026200100719     C
026300100719     C/EXEC SQL
026400100719     C+ open C_CCMDSP
026500100719     C/END-EXEC
026600100719     C
026700100719     C/EXEC SQL
026800100719     C+ Fetch C_CCMDSP into :TITAS00F
026900100719     C/END-EXEC
027000100719     C*
027100100719     C                   dow       sqlcod = *zeros
027200100719     C                   exsr      verifica
027300100719     C                   if        CHKREC = 'S'
027400100719     C                   exsr      scriviVAB
027500100719     C                   endif
027600100719     C
027700100719     C/EXEC SQL
027800100719     C+ Fetch C_CCMDSP into :TITAS00F
027900100719     C/END-EXEC
028000100719     C*
028100100719     C                   enddo
028200100719     C*
028300100719     C/EXEC SQL
028400100719     C+ close C_CCMDSP
028500100719     C/END-EXEC
028600100719     C*
028700100719     C*
028800100719     C                   ENDSR
028900100719     C*------------------------------------------------------------------------*
029000100719
029100100719
029200100719
029300100719     C*------------------------------------------------------------------------*
029400100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
029500100719     C*------------------------------------------------------------------------*
029600100719     C     SQ_KSCDSP     BEGSR
029700100719     C*
029800100719     C                   z-add     KUNI(I)       wParKSC           7 0
029900100719     C*
030000100719     C/EXEC SQL
030100100719     C+ declare C_KSCDSP cursor for
030200100719     C+ select * from titas00f
030300100719     C+ where tasksc = :wParKSC and
030400100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030500100719     C+ union
030600100719     C+ select * from titas10f
030700100719     C+ where tasksc = :wParKSC and
030800100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
030900100719     C+ union
031000100719     C+ select * from titasP0f
031100100719     C+ where tasksc = :wParKSC and
031200100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031300100719     C+ for read only
031400100719     C/END-EXEC
031500100719     C
031600100719     C/EXEC SQL
031700100719     C+ open C_KSCDSP
031800100719     C/END-EXEC
031900100719     C
032000100719     C/EXEC SQL
032100100719     C+ Fetch C_KSCDSP into :TITAS00F
032200100719     C/END-EXEC
032300100719     C*
032400100719     C                   dow       sqlcod = *zeros
032500100719     C                   exsr      verifica
032600100719     C                   if        CHKREC = 'S'
032700100719     C                   exsr      scriviVAB
032800100719     C                   endif
032900100719     C
033000100719     C/EXEC SQL
033100100719     C+ Fetch C_KSCDSP into :TITAS00F
033200100719     C/END-EXEC
033300100719     C*
033400100719     C                   enddo
033500100719     C*
033600100719     C/EXEC SQL
033700100719     C+ close C_KSCDSP
033800100719     C/END-EXEC
033900100719     C*
034000100719     C*
034100100719     C                   ENDSR
034200100719     C*------------------------------------------------------------------------*
034300100719
034400001218
034500001218
034600010131     C*------------------------------------------------------------------------*
034700010131     C* PROCIMI - Routine principale per richiesta lancio IMI
034800010131     C*------------------------------------------------------------------------*
034900081006     C     PROCIMI       BEGSR
035000010131     C*
035100010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
035200010131     C                   z-add     DATADA        DATAWRK
035300010131     C                   z-add     AAAA          depAAS
035400010131     C                   z-add     MMGG          depMGS
035500010131     C*
035600010131     C     K38C          setll     titas38c
035700010131     C                   read      titas38c
035800010131     C*
035900010131     C                   dow       not %eof(titas38c)
036000010131     C* Verifico di non essere fuori range date
036100010131     C                   z-add     tasaas        AAAA
036200010131     C                   z-add     tasmgs        MMGG
036300010131     C                   if        DATAWRK > DATAA
036400010131     C                   leave
036500010131     C                   else
036600010131     C                   exsr      verifica
036700010131     C                   if        CHKREC = 'S'
036800010131     C                   exsr      scriviVAB
036900010131     C                   endif
037000010131     C                   endif
037100010131     C                   read      titas38c
037200010131     C                   enddo
037300010131     C*
037400010131     C                   ENDSR
037500010131     C*------------------------------------------------------------------------*
037600010131
037700010131
037800001218
037900001218     C*------------------------------------------------------------------------*
038000001218     C* VERIFICA - Routine di verifica validità record corrente
038100001218     C*------------------------------------------------------------------------*
038200001218     C     VERIFICA      BEGSR
038300001218     C*
038400001218     C                   movel     'S'           CHKREC            1
038500001218     C*
038600001218     C* Verifico le date
038700001218     C                   if        CHKREC = 'S'
038800001218     C                   movel     tasaas        wrkdata           8 0
038900001218     C                   move      tasmgs        wrkdata
039000001218     C                   if        wrkdata < DATADA or
039100001218     C                             wrkdata > DATAA
039200001218     C                   movel     'N'           CHKREC
039300001218     C                   endif
039400001218     C                   endif
039500001218     C*
039600010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
039700001218     C                   if        CHKREC = 'S'
039800010131     C                   if        FLGIMI <> 'S'
039900010301     C                   if        tasksc <> KUNI(I)
040000010301     C                   movel     'N'           CHKREC
040100001218     C                   endif
040200010131     C                   endif
040300001218     C                   endif
040400010131     C*
040500010131     C* Verifico esistenza IMI qualora richiesto nel lancio
040600010131     C                   if        CHKREC = 'S'
040700010131     C                   if        FLGIMI = 'S'
040800010131     C                   if        tasftc = 'F' or tastc2 = 'F'
040900010131     C                   else
041000010131     C                   movel     'N'           CHKREC
041100010131     C                   endif
041200010131     C                   endif
041300010131     C                   endif
041400100719     C*
041500100719     C* Verifico il codice cliente mittente
041600100719     C                   if        CHKREC = 'S'
041700100719     C                   if        wChkCli  = 'C'      AND
041800100719     C                             tasccm  <> KUNI(I)
041900100719     C                   movel     'N'           CHKREC
042000100719     C                   endif
042100100719     C                   endif
042200100719     C*
042300100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
042400100719     C                   if        CHKREC = 'S'
042500100719     C                   if        wChkCli  = 'K'      AND
042600100719     C                             tasksc  <> KUNI(I)
042700100719     C                   movel     'N'           CHKREC
042800100719     C                   endif
042900100719     C                   endif
043000100719     C*
043100100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
043200100719     C                   if        CHKREC = 'S'
043300100719     C                   if        TIPCLI  = 'E'       AND
043400100719     C                             wChkCli = 'K'       AND
043500100719     C                             tasksc  = tasccm
043600100719     C                   movel     'N'           CHKREC
043700100719     C                   endif
043800100719     C                   endif
043900140702     C*
044000090217     C* Verifico il tipo bolla che non deve essere di reupero
044100090217     C                   if        CHKREC = 'S'
044200090217     C                   if        %lookup(tastbl:tbo)>0
044300090217     C                   movel     'N'           CHKREC
044400090217     C                   endif
044500090217     C                   endif
044600001218     C*
044700001218     C                   ENDSR
044800001218     C*------------------------------------------------------------------------*
044900001218
045000001218
045100001218
045200001218     C*------------------------------------------------------------------------*
045300001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
045400001218     C*------------------------------------------------------------------------*
045500001221     C     SCRIVIVAB     BEGSR
045600001218     C*
045700001227     C                   clear                   fnvab000
045800010131     C                   if        FLGIMI = 'S'
045900010131     C                   seton                                        55
046000010131     C                   else
046100010131     C                   setoff                                       55
046200010131     C                   endif
046300001218     C*
046400001221     C                   movel     tascbo        vabcbo
046500001221     C                   movel     tasrsd        vabrsd
046600001221     C                   movel     tasind        vabind
046700001221     C                   movel     tascad        vabcad
046800001221     C                   movel     taslod        vablod
046900001221     C                   movel     tasprd        vabprd
047000001221     C                   movel     tasnzd        vabnzd
047100001221     C                   movel     tasgc1        vabgc1
047200001221     C                   movel     tasgc2        vabgc2
047300001221     C                   movel     tastsp        vabtsp
047400001221     C                   movel     tasnas        vabnas
047500001221     C                   movel     tasctm        vabctm
047600001221     C                   movel     tasffd        vabffd
047700001221     C                   movel     tastcr        vabtcr
047800001221     C                   movel     tascts        vabcts
047900001221     C                   movel     tasftm        vabftm
048000001221     C                   movel     tasgma        vabgma
048100001221     C                   movel     tasgga        vabgga
048200001221     C                   movel     tasgva        vabgva
048300010111     C                   move      ' '           vabgva
048400100927     C                   movel     tasftc        vabtc1
048500100927     C                   movel     tastc2        vabtc2
048600001221     C                   if        tasccm > 0
048700001221     C                   z-add     tasccm        vabccm
048800001221     C                   else
048900001221     C                   z-add     tasksc        vabccm
049000001221     C                   end
049100001221     C                   z-add     taslnp        vablnp
049200001221     C                   z-add     tasaas        vabaas
049300001221     C                   z-add     tasmgs        vabmgs
049400001221     C                   z-add     tasnrs        vabnrs
049500001221     C                   z-add     tasnsp        vabnsp
049600001221     C                   z-add     taslna        vablna
049700001221     C                   z-add     tasctr        vabctr
049800001221     C                   z-add     tasncl        vabncl
049900001221     C                   z-add     taspkf        vabpkb
050000001221     C                   z-add     tasvlf        vabvlb
050100001221     C                   z-add     tasqft        vabqft
050200001221     C                   z-add     tasrmn        vabrmn
050300001221     C                   z-add     tasncd        vabncd
050400001221     C                   z-add     tasnca        vabnca
050500001221     C                   z-add     tasznc        vabznc
050600010108     C                   z-add     tasdbr        vabdcr
050700001221     C* Effettuo considerazioni particolare per bolla "POSTE"
050800001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
050900001227     C                   exsr      REPPTIAS
051000001227     C                   else
051100001227     C                   movel     tasIAS        vabIAS
051200001227     C                   movel     tasVAS        vabVAS
051300001227     C                   movel     tasXCO        vabXCO
051400001227     C                   endif
051500001227     C* Considerazioni per bolle "autogenerate"
051600001227     C                   if        tasSOP = '&' and
051700001227     C                             vabSCL = *blanks
051800001227     C                   movel     'A'           vabSCL
051900001227     C                   endif
052000001227     C*
052100001227     C                   exsr      REPCSB
052200010131     C                   exsr      REPRMA
052300081006     C                   exsr      REPANT
052400081006     C                   exsr      REPDEST
052500010605     C*
052600010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
052700010605     C                   exsr      REPIMP
052800001218     C*
052900010131     C                   if        *in55 = *off
053000050825     C   25              WRITE     FNVAB000
053100050825     C   26              exsr      WRITIVGD
053200010131     C                   endif
053300001218     C*
053400001218     C                   ENDSR
053500001221     C*------------------------------------------------------------------------*
053600050825
053700050825
053800050825
053900050825
054000050825     C*------------------------------------------------------------------------*
054100050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
054200050825     C*------------------------------------------------------------------------*
054300050825     C     WRITIVGD      BEGSR
054400050825     C*
054500050825     C                   clear                   tivgd000
054600080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
054700100312     C                   eval      vgdTIP = TIPFILE
054800050825     C                   movel     *all'0'       vgdKSU
054900080115     C                   move      CODCLIVAS     vgdKSU
055000050825     C                   eval      vgdTSC = 'WW'
055100050825     C                   eval      vgdDAT = datcor
055200050825     C                   eval      vgdPGM = 'TIS7VBR'
055300080213     C*
055400080213     C                   clear                   DVGDFLO
055500080213     C                   movel     'P'           §VGDFELA
055600080213     C                   movel     DVGDFLO       vgdflo
055700080115     C*
055800050825     C                   write     tivgd000
055900050825     C*
056000050825     C                   ENDSR
056100050825     C*------------------------------------------------------------------------*
056200080213
056300080213
056400080213
056500080213     C*------------------------------------------------------------------------*
056600080213     C* FORZADWN - Routine di forzatura elaborazione immediata download
056700080213     C*------------------------------------------------------------------------*
056800080213     C     FORZADWN      BEGSR
056900080213     C*
057000080213     C                   clear                   tis781ds
057100080213     C                   eval      §781tip = vgdTIP
057200080213     C                   eval      §781ksu = vgdKSU
057300080213     C                   eval      §781tsc = vgdTSC
057400080213     C                   eval      §781dat = vgdDAT
057500080213     C                   eval      §781pgm = vgdPGM
057600080905     C*
057700080905     C                   clear                   TIS781DFLO
057800080905     C                   movel     'P'           §781floela
057900080905     C                   movel     TIS781DFLO    §781flo
058000080213     C*
058100080213     C* Finita l'elaborazione chiamo pgm
058200080213     C                   CALL      'TIS781C1'
058300080213     C                   parm      *blanks       esito             1
058400080213     C                   parm                    tis781ds
058500080213     C                   parm      *blanks       vgdPRG           10
058600080213     C*
058700080213     C                   ENDSR
058800080213     C*------------------------------------------------------------------------*
058900001218
059000001221
059100001221
059200001218
059300001218     C*------------------------------------------------------------------------*
059400001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
059500010131     C*          - e dei pacchi ingombranti non dichiarati
059600001218     C*------------------------------------------------------------------------*
059700001218     C     REPPTIAS      BEGSR
059800001218     C*
059900060215     D* Prendo importo da assicurare da TITA4 t.r.=J
060000001218     C                   movel     'J'           KTRC
060100001218     C     KTA4          chain     tita430c
060200001218     C                   if        %found(tita430c)
060300001218     C                   movel     ta4NOT        DSBL4J
060400001221     C                   z-add     §B4IAS        vabIAS
060500001221     C                   movel     §B4VAS        vabVAS
060600001221     C                   movel     §B4XCO        vabXCO
060700001227     C                   movel     §B4BAU        vabSCL
060800010131     C                   if        §B4BAI = 'S'
060900010131     C                   movel     §B4BAI        vabFTM
061000010131     C                   setoff                                       55
061100010131     C                   endif
061200001218     C                   endif
061300001218     C*
061400001218     C                   ENDSR
061500001218     C*------------------------------------------------------------------------*
061600001218
061700001218
061800001218
061900001218
062000001218     C*------------------------------------------------------------------------*
062100001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
062200001218     C*------------------------------------------------------------------------*
062300001218     C     REPRMA        BEGSR
062400001218     C*
062500060215     D* Prendo importo da assicurare da TITA4 t.r.=A
062600001218     C                   movel     'A'           KTRC
062700001218     C     KTA4          chain     tita430c
062800001218     C                   if        %found(tita430c)
062900001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
063000001218     C                   endif
063100001218     C*
063200001218     C                   ENDSR
063300001218     C*------------------------------------------------------------------------*
063400001218
063500001218
063600001218
063700001218
063800001218     C*------------------------------------------------------------------------*
063900001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
064000001218     C*           ... e ai dati del mittente originale
064100001218     C*------------------------------------------------------------------------*
064200001218     C     REPDEST       BEGSR
064300001218     C*
064400001218     D* Prendo il tipo anagrafica = D
064500001218     C                   movel     'D'           KTRC
064600010131     C     KTAA          chain     titaa30c
064700001218     C                   if        %found(titaa30c)
064800001221     C                   movel     taaRSC        vabRD2
064900001218     C                   endif
065000001218     C*
065100001221     D* Prendo il tipo anagrafica = O
065200001218     C                   movel     'O'           KTRC
065300001218     C     KTAA          chain     titaa30c
065400001218     C                   if        %found(titaa30c)
065500001221     C                   movel     taaRSC        vabRMO
065600120326     C***                movel     taaCAP        vabCMO
065700120326     C***                movel     taaNAZ        vabNMO
065800001218     C                   endif
065900001222     C                   if        vabNMO = *blanks
066000001222     C                   move      '.'           vabNMO
066100001222     C                   endif
066200001218     C*
066300001218     C                   ENDSR
066400001218     C*------------------------------------------------------------------------*
066500001218
066600001218
066700001218
066800001218
066900001218     C*------------------------------------------------------------------------*
067000001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
067100001218     C*------------------------------------------------------------------------*
067200001218     C     REPCSB        BEGSR
067300001218     C*
067400001219     C     KCSB          chain     tncsb03l
067500001219     C                   if        %found(tncsb03l)
067600001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
067700001218     C                   else
067800001221     C                   movel     csbTPA        vabTIC
067900001221     C                   move      csbTPI        vabTIC
068000001221     C                   z-add     csbCAS        vabCAS
068100001221     C                   movel     csbVCA        vabVCA
068200001221     C                   movel     csbGCA        vabGCA
068300001218     C                   endif
068400001218     C                   endif
068500001218     C*
068600001218     C                   ENDSR
068700001218     C*------------------------------------------------------------------------*
068800001218
068900010605
069000010605
069100010605
069200010605     C*------------------------------------------------------------------------*
069300010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
069400010605     C*------------------------------------------------------------------------*
069500010605     C     REPIMP        BEGSR
069600010605     C*
069700010605     C* Come prima cosa verifico se trattasi di bolla POSTE
069800010605     C                   if        vabtsp = 'P'
069900010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
070000010605     C                   if        vabvas = 'EUR'
070100010605     C                   eval      vabvmd = vabias
070200010605     C                   eval(h)   wrklire = vabvmd * 1936,27
070300010605     C                   eval      vabias = wrklire
070400010605     C                   eval      vabvas = *blanks
070500010605     C                   endif
070600010605     C*
070700010605     C                   if        vabvca = 'EUR'
070800010605     C                   eval      vabqft = vabcas
070900010605     C                   eval(h)   wrklire = vabqft * 1936,27
071000010605     C                   eval      vabcas = wrklire
071100010605     C                   eval      vabvca = *blanks
071200010605     C                   endif
071300010605     C*
071400010605     C                   eval      vabxco = *blanks
071500010605     C                   endif
071600010605     C*
071700010605     C                   ENDSR
071800030704
071900030704
072000030704
072100030704
072200030704     C*------------------------------------------------------------------------*
072300030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
072400030704     C*------------------------------------------------------------------------*
072500030704     C     REPANT        BEGSR
072600030704     C*
072700050825     C                   clear                   ds_vabANT
072800030704     C                   if        %subst(tasGVA:1:1) = 'B'
072900040531     C     KFIAR5        chain     fiar531c
073000040531     C                   if        %found(fiar531c)
073100030704     C                   movel     ar5UNI        DAR5BAN
073200030704     C                   movel     §ar5TBA       vabTBA
073300030704     C                   movel     §ar5NBA       vabNBA
073400030704     C                   movel     §ar5TB2       vabTB2
073500030704     C                   movel     §ar5NB2       vabNB2
073600030704     C                   endif
073700030704     C                   endif
073800050825     C                   eval      vabANT = ds_vabANT
073900030704     C*
074000030704     C                   ENDSR
074100030704     C*------------------------------------------------------------------------*
074200010605
074300010605
074400010605
074500001218
074600001218
074700001218     C*------------------------------------------------------------------------*
074800001218     C* CARTBL - Routine di caricamento dati tabellati
074900001218     C*------------------------------------------------------------------------*
075000001218     C     CARTBL        BEGSR
075100001218     C*
075200001221     C                   Z-ADD     0             I                 4 0
075300050825     C                   IF        FLGUNI = 'S'
075400010301     C                   ADD       1             I
075500050825     C                   MOVEL     CODCLI        KUNI(I)
075600010301     C                   ELSE
075700001221     C                   MOVEL     '3Q'          COD
075800001218     C     KTAB          CHAIN     TABEL                              31
075900001218     C     *IN31         DOWEQ     '0'
076000090518     C                   IF        TBLFLG = ' '
076100090518     C***                IF        TBLFLG = ' ' AND
076200090518     C***                          %subst(TBLKEY:8:1) = ' '
076300001222     C                   MOVEL     TBLUNI        DS3Q
076400050825     C     §3QCKS        IFEQ      CODCLI
076500001221     C                   ADD       1             I
076600001221     C                   MOVEL     TBLKEY        KUNI(I)
076700001221     C                   ENDIF
076800001218     C                   ENDIF
076900001218     C     KTAB          READE     TABEL                                  31
077000001218     C                   ENDDO
077100010301     C                   ENDIF
077200120326     c* carico schiera tipi bolla nn validi ai fini della spedizione
077300090217     c                   clear                   k
077400090217     c                   movel     'TB'          cod
077500090217     c     ktab          setll     tabel00f
077600090217     c     ktab          reade     tabel00f
077700090217     c                   dow       not %eof(tabel00f)
077800090217     c                   if        tblflg<>'*'
077900090217     C                   movel     tbluni        dstb
078000140702     C                   IF        §tbrbl='R'
078100090217     C                   ADD       1             K
078200090217     C                   MOVEL     TBLKEY        TBO(K)
078300090217     c                   endif
078400140702     C                   IF        §tbrbl='C' and FLGSICS <> 'S'
078500140702     C                   ADD       1             K
078600140702     C                   MOVEL     TBLKEY        TBO(K)
078700140702     c                   endif
078800090217     c                   endif
078900090217     c     ktab          reade     tabel00f
079000090217     c                   enddo
079100090217     c
079200001218     C*
079300001218     C                   ENDSR
079400001221     C*------------------------------------------------------------------------*
079500001218
079600001218
079700001218
079800001218
079900001218     C*------------------------------------------------------------------------*
080000001218     C* *INZSR - ROUTINE INIZIALE
080100001218     C*------------------------------------------------------------------------*
080200001218     C     *INZSR        BEGSR
080300001218     C*
080400001218     C     *ENTRY        PLIST
080500100312     C                   PARM                    PARAM            37
080600001218     C*
080700001218     C                   MOVEL     PARAM         DSINPUT
080800001218     C                   Z-ADD     1             CODUT
080900001218     C*
081000001218     C* Definizioni chiavi
081100010131     C     K38C          KLIST
081200010131     C                   KFLD                    depAAS            4 0
081300010131     C                   KFLD                    depMGS            4 0
081400010131     C*
081500001218     C     KTA4          KLIST
081600001218     C                   KFLD                    tasAAS
081700001218     C                   KFLD                    tasLNP
081800001218     C                   KFLD                    tasNRS
081900001218     C                   KFLD                    tasNSP
082000001218     C                   KFLD                    KTRC              1
082100001218     C*
082200001218     C     KTAA          KLIST
082300001218     C                   KFLD                    tasAAS
082400001218     C                   KFLD                    tasLNP
082500001218     C                   KFLD                    tasNRS
082600001218     C                   KFLD                    tasNSP
082700001218     C                   KFLD                    KTRC
082800001218     C*
082900001218     C     KCSB          KLIST
083000001218     C                   KFLD                    tasAAS
083100001218     C                   KFLD                    tasLNP
083200001218     C                   KFLD                    tasNRS
083300001218     C                   KFLD                    tasNSP
083400001218     C*
083500001218     C     KTAB          KLIST
083600001218     C                   KFLD                    CODUT             1 0
083700001218     C                   KFLD                    COD               2
083800030704     C*
083900030704     C     KFIAR5        KLIST
084000030704     C                   KFLD                    tasAAS
084100030704     C                   KFLD                    tasLNP
084200030704     C                   KFLD                    tasNRS
084300030704     C                   KFLD                    tasNSP
084400030704     C                   KFLD                    kar5TRD
084500001218     C*
084600001218     C* Determino la data corrente
084700100312     C                   Z-ADD     *zeros        DATCOR            8 0
084800100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
084900001218     C*
085000001218     C                   ENDSR
