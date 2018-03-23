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
003600160208     D DTA4A         E DS
003700001218     D*--------------------
003800001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003900001218     D*--------------------
004000050825     D DSINPUT         DS
004100050825     D  DATADA                        8  0
004200050825     D  DATAA                         8  0
004300050825     D  CODCLI                        7  0
004400050825     D  FLGIMI                        1
004500050825     D  FLGUNI                        1
004600050825     D  FLGOPZ                        1
004700080115     D  CODCLIVAS                     7  0
004800100312     D  TIPFILE                       2
004900080213     D  FLGEXE                        1
005000080213     D  TIPCLI                        1
005100140702     D  FLGSICS                       1
005200010131     D*--------------------
005300010131     D* DS DI SCOMPOSIZIONE DATE
005400010131     D*--------------------
005500010131     D                 DS
005600010131     D  AAAA                   1      4  0
005700010131     D  MMGG                   5      8  0
005800010131     D  DATAWRK                1      8  0
005900030704     D*--------------------
006000030704     D* DS DI RIDEFINIZIONE FIAR5 TIPO RECORD "BAN"
006100030704     D*--------------------
006200030704     D DAR5BAN       E DS
006300151019     D DAR5FAT       E DS
006400030704     D*--------------------
006500030704     D* DS DI SCOMPOSIZIONE CAMPO VABANT
006600030704     D*--------------------
006700030704     D                 DS
006800030704     D vabTBA                  1      2  0
006900030704     D vabNBA                  3      4  0
007000030704     D vabTB2                  5      6  0
007100030704     D vabNB2                  7      8  0
007200050825     D ds_vabANT               1      9  0
007300010605     D*--------------------
007400010605     D* VARIABILI DI WRK
007500010605     D*--------------------
007600030704     D wrklire         S             10  0
007700030704     D kar5TRD         S                   Like(ar5TRD) Inz('BAN')
007800090217     D K               S              4  0
007900160208     D*--------------------
008000160208     D* DEFINIZIONI A PROCEDURE ESTERNE
008100160208     D*--------------------
008200160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008300001218
008400010605
008500010605
008600920812     C*---------------------------------------------------------------*
008700001218     C* MAIN
008800001218     C*---------------------------------------------------------------*
008900100719     C*
009000100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009100100719     C
009200100719     C/EXEC SQL
009300100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009400100719     C/END-EXEC
009500100719     C
009600100719     C*
009700001218     C                   exsr      cartbl
009800001218     C                   exsr      chkpar
009900050909     C*
010000050909     C* Inizializzo variabili d wrk
010100050909     C                   movel     'N'           wProcedi          1
010200050909     C*
010300050909     C* Se richiesta scrittura sul file 00T => elaboro
010400050909     C                   if        *in25 = *on
010500050909     C                   open      fnvab00t
010600050909     C                   movel     'S'           wProcedi
010700050909     C                   endif
010800050909     C*
010900050909     C* Se richiesta scrittura sul file TIVGD00F => elaboro
011000050909     C* e come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente: 'BT'
011100050909     C                   if        *in26 = *on
011200050909     C                   open      tivgd00f
011300050909     C                   clear                   trul47ds
011400050909     C                   eval      d47opz  = 'I'
011500120927     C                   eval      d47tip  = TIPFILE
011600050909     C                   eval      d47lck  = 'N'
011700050909     C                   eval      d47chkj = 'N'
011800050909     C                   eval      d47pgm  = 'TIS7VBR'
011900050909     C                   call      'TRUL47R'
012000050909     C                   parm                    trul47ds
012100050909     C*
012200050909     C* Se elaborazione consentita => proseguo
012300120928     C***                if        d47sts <> 'A'
012400050909     C                   movel     'S'           wProcedi
012500120928     C***                endif
012600050909     C                   endif
012700050909     C*
012800050909     C* Se ok a procedere => elaboro
012900050909     C                   if        wProcedi = 'S'
013000010131     C                   if        FLGIMI <> 'S'
013100001218     C                   exsr      procedi
013200010131     C                   else
013300010131     C                   exsr      procimi
013400010131     C                   endif
013500050909     C                   endif
013600050909     C*
013700050909     C* Chiudo il file di output
013800050909     C   25              close     fnvab00t
013900050909     C   26              close     tivgd00f
014000050909     C*
014100050909     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
014200050909     C                   clear                   trul47ds
014300050909     C                   eval      d47opz  = 'F'
014400120928     C                   eval      d47tip  = TIPFILE
014500050909     C                   call      'TRUL47R'
014600050909     C                   parm                    trul47ds
014700160208     C*
014800160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
014900160208     C                   CALL      'UBTA400R'
015000160208     C                   PARM                    UBTA4IOPZ
015100160208     C                   PARM      'C'           UBTA4ITLA
015200160208     C                   PARM                    UBTA4IAAS
015300160208     C                   PARM                    UBTA4ILNP
015400160208     C                   PARM                    UBTA4INRS
015500160208     C                   PARM                    UBTA4INSP
015600160208     C                   PARM                    UBTA4ITRC
015700160208     C                   PARM                    UBTA4OERR
015800160208     C                   PARM                    UBTA4ODS
015900160208     C                   PARM                    UBTA4OLEN
016000160208     C                   PARM                    UBTA4ODATI
016100080213     C*
016200080213     C                   if        FLGOPZ = '2'
016300161205     C***                exsr      forzaDWN
016400080213     C                   endif
016500001218     C*
016600001218     C                   seton                                        LR
016700001218
016800001218
016900001218
017000001218
017100001218     C*------------------------------------------------------------------------*
017200001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
017300001218     C*------------------------------------------------------------------------*
017400001218     C     CHKPAR        BEGSR
017500001218     C*
017600001218     C* Verifico le date
017700001218     C                   if        DATAA  = *zeros
017800001218     C                   movel     *all'9'       DATAA
017900001218     C                   endif
018000050825     C*
018100050825     C* Verifico la modalità d scrittura output richiesta:
018200050825     C*   1 = crea membro "RIC" in file FNVAB00T
018300050825     C*   2 = scrivi record in file TIVGD00F
018400050825     C                   setoff                                       2526
018500050825     C                   if        FLGOPZ = '1'
018600050825     C                   seton                                        25
018700050825     C                   endif
018800050825     C                   if        FLGOPZ = '2'
018900050825     C                   seton                                        26
019000050825     C                   endif
019100001218     C*
019200001218     C                   ENDSR
019300001218     C*------------------------------------------------------------------------*
019400001218
019500001218
019600001218
019700001218     C*------------------------------------------------------------------------*
019800001218     C* PROCEDI - Routine principale
019900001218     C*------------------------------------------------------------------------*
020000001218     C     PROCEDI       BEGSR
020100001218     C*
020200001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
020300001218     C                   z-add     1             I
020400090407     C                   sorta     KUNI
020500001218     C                   dow       KUNI(I) > *zeros
020600100719     C*
020700100719     C* Se richiesta elaborazione x codice cliente mittente...
020800100719     C                   select
020900100719     C                   when      TIPCLI = *blanks
021000100719     C                   exsr      EXECCM
021100100719     C*
021200100719     C* Se richiesta elaborazione x codice cliente fatturazione...
021300100719     C                   when      TIPCLI = 'K'
021400100719     C                   exsr      EXEKSC
021500100719     C*
021600100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
021700100719     C                   when      TIPCLI = 'E'
021800100719     C                   exsr      EXECCM
021900100719     C                   exsr      EXEKSC
022000100719     C*
022100100719     C                   endsl
022200001218     C*
022300001218     C                   add       1             I
022400001218     C                   enddo
022500001218     C*
022600001218     C                   ENDSR
022700001218     C*------------------------------------------------------------------------*
022800100719
022900100719
023000100719
023100100719     C*------------------------------------------------------------------------*
023200100719     C* EXECCM   - Elaborazione x codice cliente mittente
023300100719     C*------------------------------------------------------------------------*
023400100719     C     EXECCM        BEGSR
023500100719     C*
023600100719     C                   movel     'C'           wChkCli           1
023700100719     C*
023800100719     C                   exsr      SQ_CCMDSP
023900100719     C*
024000100719     C                   ENDSR
024100100719     C*------------------------------------------------------------------------*
024200100719
024300100719
024400100719
024500100719     C*------------------------------------------------------------------------*
024600100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
024700100719     C*------------------------------------------------------------------------*
024800100719     C     EXEKSC        BEGSR
024900100719     C*
025000100719     C                   movel     'K'           wChkCli           1
025100100719     C*
025200100719     C                   exsr      SQ_KSCDSP
025300100719     C*
025400100719     C                   ENDSR
025500100719     C*------------------------------------------------------------------------*
025600100719
025700100719
025800100719
025900100719     C*------------------------------------------------------------------------*
026000100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
026100100719     C*------------------------------------------------------------------------*
026200100719     C     SQ_CCMDSP     BEGSR
026300100719     C*
026400100719     C                   z-add     KUNI(I)       wParCCM           7 0
026500100719     C*
026600100719     C/EXEC SQL
026700100719     C+ declare C_CCMDSP cursor for
026800100719     C+ select * from titas00f
026900100719     C+ where tasccm = :wParCCM and
027000100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027100100719     C+ union
027200100719     C+ select * from titas10f
027300100719     C+ where tasccm = :wParCCM and
027400100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027500100719     C+ union
027600100719     C+ select * from titasP0f
027700100719     C+ where tasccm = :wParCCM and
027800100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027900100719     C+ for read only
028000100719     C/END-EXEC
028100100719     C
028200100719     C/EXEC SQL
028300100719     C+ open C_CCMDSP
028400100719     C/END-EXEC
028500100719     C
028600100719     C/EXEC SQL
028700100719     C+ Fetch C_CCMDSP into :TITAS00F
028800100719     C/END-EXEC
028900100719     C*
029000100719     C                   dow       sqlcod = *zeros
029100100719     C                   exsr      verifica
029200100719     C                   if        CHKREC = 'S'
029300100719     C                   exsr      scriviVAB
029400100719     C                   endif
029500100719     C
029600100719     C/EXEC SQL
029700100719     C+ Fetch C_CCMDSP into :TITAS00F
029800100719     C/END-EXEC
029900100719     C*
030000100719     C                   enddo
030100100719     C*
030200100719     C/EXEC SQL
030300100719     C+ close C_CCMDSP
030400100719     C/END-EXEC
030500100719     C*
030600100719     C*
030700100719     C                   ENDSR
030800100719     C*------------------------------------------------------------------------*
030900100719
031000100719
031100100719
031200100719     C*------------------------------------------------------------------------*
031300100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
031400100719     C*------------------------------------------------------------------------*
031500100719     C     SQ_KSCDSP     BEGSR
031600100719     C*
031700100719     C                   z-add     KUNI(I)       wParKSC           7 0
031800100719     C*
031900100719     C/EXEC SQL
032000100719     C+ declare C_KSCDSP cursor for
032100100719     C+ select * from titas00f
032200100719     C+ where tasksc = :wParKSC and
032300100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032400100719     C+ union
032500100719     C+ select * from titas10f
032600100719     C+ where tasksc = :wParKSC and
032700100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032800100719     C+ union
032900100719     C+ select * from titasP0f
033000100719     C+ where tasksc = :wParKSC and
033100100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
033200100719     C+ for read only
033300100719     C/END-EXEC
033400100719     C
033500100719     C/EXEC SQL
033600100719     C+ open C_KSCDSP
033700100719     C/END-EXEC
033800100719     C
033900100719     C/EXEC SQL
034000100719     C+ Fetch C_KSCDSP into :TITAS00F
034100100719     C/END-EXEC
034200100719     C*
034300100719     C                   dow       sqlcod = *zeros
034400100719     C                   exsr      verifica
034500100719     C                   if        CHKREC = 'S'
034600100719     C                   exsr      scriviVAB
034700100719     C                   endif
034800100719     C
034900100719     C/EXEC SQL
035000100719     C+ Fetch C_KSCDSP into :TITAS00F
035100100719     C/END-EXEC
035200100719     C*
035300100719     C                   enddo
035400100719     C*
035500100719     C/EXEC SQL
035600100719     C+ close C_KSCDSP
035700100719     C/END-EXEC
035800100719     C*
035900100719     C*
036000100719     C                   ENDSR
036100100719     C*------------------------------------------------------------------------*
036200100719
036300001218
036400001218
036500010131     C*------------------------------------------------------------------------*
036600010131     C* PROCIMI - Routine principale per richiesta lancio IMI
036700010131     C*------------------------------------------------------------------------*
036800081006     C     PROCIMI       BEGSR
036900010131     C*
037000010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
037100010131     C                   z-add     DATADA        DATAWRK
037200010131     C                   z-add     AAAA          depAAS
037300010131     C                   z-add     MMGG          depMGS
037400010131     C*
037500010131     C     K38C          setll     titas38c
037600010131     C                   read      titas38c
037700010131     C*
037800010131     C                   dow       not %eof(titas38c)
037900010131     C* Verifico di non essere fuori range date
038000010131     C                   z-add     tasaas        AAAA
038100010131     C                   z-add     tasmgs        MMGG
038200010131     C                   if        DATAWRK > DATAA
038300010131     C                   leave
038400010131     C                   else
038500010131     C                   exsr      verifica
038600010131     C                   if        CHKREC = 'S'
038700010131     C                   exsr      scriviVAB
038800010131     C                   endif
038900010131     C                   endif
039000010131     C                   read      titas38c
039100010131     C                   enddo
039200010131     C*
039300010131     C                   ENDSR
039400010131     C*------------------------------------------------------------------------*
039500010131
039600010131
039700001218
039800001218     C*------------------------------------------------------------------------*
039900001218     C* VERIFICA - Routine di verifica validità record corrente
040000001218     C*------------------------------------------------------------------------*
040100001218     C     VERIFICA      BEGSR
040200001218     C*
040300001218     C                   movel     'S'           CHKREC            1
040400001218     C*
040500001218     C* Verifico le date
040600001218     C                   if        CHKREC = 'S'
040700001218     C                   movel     tasaas        wrkdata           8 0
040800001218     C                   move      tasmgs        wrkdata
040900001218     C                   if        wrkdata < DATADA or
041000001218     C                             wrkdata > DATAA
041100001218     C                   movel     'N'           CHKREC
041200001218     C                   endif
041300001218     C                   endif
041400001218     C*
041500010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
041600001218     C                   if        CHKREC = 'S'
041700010131     C                   if        FLGIMI <> 'S'
041800010301     C                   if        tasksc <> KUNI(I)
041900010301     C                   movel     'N'           CHKREC
042000001218     C                   endif
042100010131     C                   endif
042200001218     C                   endif
042300010131     C*
042400010131     C* Verifico esistenza IMI qualora richiesto nel lancio
042500010131     C                   if        CHKREC = 'S'
042600010131     C                   if        FLGIMI = 'S'
042700010131     C                   if        tasftc = 'F' or tastc2 = 'F'
042800010131     C                   else
042900010131     C                   movel     'N'           CHKREC
043000010131     C                   endif
043100010131     C                   endif
043200010131     C                   endif
043300100719     C*
043400100719     C* Verifico il codice cliente mittente
043500100719     C                   if        CHKREC = 'S'
043600100719     C                   if        wChkCli  = 'C'      AND
043700100719     C                             tasccm  <> KUNI(I)
043800100719     C                   movel     'N'           CHKREC
043900100719     C                   endif
044000100719     C                   endif
044100100719     C*
044200100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
044300100719     C                   if        CHKREC = 'S'
044400100719     C                   if        wChkCli  = 'K'      AND
044500100719     C                             tasksc  <> KUNI(I)
044600100719     C                   movel     'N'           CHKREC
044700100719     C                   endif
044800100719     C                   endif
044900100719     C*
045000100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
045100100719     C                   if        CHKREC = 'S'
045200100719     C                   if        TIPCLI  = 'E'       AND
045300100719     C                             wChkCli = 'K'       AND
045400100719     C                             tasksc  = tasccm
045500100719     C                   movel     'N'           CHKREC
045600100719     C                   endif
045700100719     C                   endif
045800140702     C*
045900090217     C* Verifico il tipo bolla che non deve essere di reupero
046000090217     C                   if        CHKREC = 'S'
046100090217     C                   if        %lookup(tastbl:tbo)>0
046200090217     C                   movel     'N'           CHKREC
046300090217     C                   endif
046400090217     C                   endif
046500001218     C*
046600001218     C                   ENDSR
046700001218     C*------------------------------------------------------------------------*
046800001218
046900001218
047000001218
047100001218     C*------------------------------------------------------------------------*
047200001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
047300001218     C*------------------------------------------------------------------------*
047400001221     C     SCRIVIVAB     BEGSR
047500001218     C*
047600001227     C                   clear                   fnvab000
047700010131     C                   if        FLGIMI = 'S'
047800010131     C                   seton                                        55
047900010131     C                   else
048000010131     C                   setoff                                       55
048100010131     C                   endif
048200151019     C*
048300151019     C* Eseguo eventuali "forzature"
048400151019     C                   EXSR      RTVAR5FAT
048500160208     C*
048600160208     C* Reprrisco NATURA MERCE
048700160208     C                   EXSR      RTVNAS
048800151019     C*
048900001221     C                   movel     tascbo        vabcbo
049000001221     C                   movel     tasrsd        vabrsd
049100001221     C                   movel     tasind        vabind
049200001221     C                   movel     tascad        vabcad
049300001221     C                   movel     taslod        vablod
049400001221     C                   movel     tasprd        vabprd
049500001221     C                   movel     tasnzd        vabnzd
049600001221     C                   movel     tasgc1        vabgc1
049700001221     C                   movel     tasgc2        vabgc2
049800001221     C                   movel     tastsp        vabtsp
049900160208     C                   movel     §TA4ANAS      vabnas
050000001221     C                   movel     tasctm        vabctm
050100001221     C                   movel     tasffd        vabffd
050200001221     C                   movel     tastcr        vabtcr
050300001221     C                   movel     tascts        vabcts
050400001221     C                   movel     tasftm        vabftm
050500001221     C                   movel     tasgma        vabgma
050600001221     C                   movel     tasgga        vabgga
050700001221     C                   movel     tasgva        vabgva
050800010111     C                   move      ' '           vabgva
050900100927     C                   movel     tasftc        vabtc1
051000100927     C                   movel     tastc2        vabtc2
051100001221     C                   if        tasccm > 0
051200001221     C                   z-add     tasccm        vabccm
051300001221     C                   else
051400001221     C                   z-add     tasksc        vabccm
051500001221     C                   end
051600001221     C                   z-add     taslnp        vablnp
051700001221     C                   z-add     tasaas        vabaas
051800001221     C                   z-add     tasmgs        vabmgs
051900001221     C                   z-add     tasnrs        vabnrs
052000001221     C                   z-add     tasnsp        vabnsp
052100001221     C                   z-add     taslna        vablna
052200001221     C                   z-add     tasctr        vabctr
052300001221     C                   z-add     tasncl        vabncl
052400001221     C                   z-add     taspkf        vabpkb
052500001221     C                   z-add     tasvlf        vabvlb
052600001221     C                   z-add     tasqft        vabqft
052700001221     C                   z-add     tasrmn        vabrmn
052800001221     C                   z-add     tasncd        vabncd
052900001221     C                   z-add     tasnca        vabnca
053000001221     C                   z-add     tasznc        vabznc
053100010108     C                   z-add     tasdbr        vabdcr
053200001221     C* Effettuo considerazioni particolare per bolla "POSTE"
053300001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
053400001227     C                   exsr      REPPTIAS
053500001227     C                   else
053600001227     C                   movel     tasIAS        vabIAS
053700001227     C                   movel     tasVAS        vabVAS
053800001227     C                   movel     tasXCO        vabXCO
053900001227     C                   endif
054000001227     C* Considerazioni per bolle "autogenerate"
054100001227     C                   if        tasSOP = '&' and
054200001227     C                             vabSCL = *blanks
054300001227     C                   movel     'A'           vabSCL
054400001227     C                   endif
054500001227     C*
054600001227     C                   exsr      REPCSB
054700010131     C                   exsr      REPRMA
054800081006     C                   exsr      REPANT
054900081006     C                   exsr      REPDEST
055000010605     C*
055100010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
055200010605     C                   exsr      REPIMP
055300001218     C*
055400010131     C                   if        *in55 = *off
055500050825     C   25              WRITE     FNVAB000
055600050825     C   26              exsr      WRITIVGD
055700010131     C                   endif
055800001218     C*
055900001218     C                   ENDSR
056000001221     C*------------------------------------------------------------------------*
056100050825
056200050825
056300050825
056400050825
056500050825     C*------------------------------------------------------------------------*
056600050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
056700050825     C*------------------------------------------------------------------------*
056800050825     C     WRITIVGD      BEGSR
056900050825     C*
057000050825     C                   clear                   tivgd000
057100080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
057200100312     C                   eval      vgdTIP = TIPFILE
057300050825     C                   movel     *all'0'       vgdKSU
057400080115     C                   move      CODCLIVAS     vgdKSU
057500050825     C                   eval      vgdTSC = 'WW'
057600050825     C                   eval      vgdDAT = datcor
057700050825     C                   eval      vgdPGM = 'TIS7VBR'
057800080213     C*
057900080213     C                   clear                   DVGDFLO
058000080213     C                   movel     'P'           §VGDFELA
058100080213     C                   movel     DVGDFLO       vgdflo
058200080115     C*
058300050825     C                   write     tivgd000
058400050825     C*
058500050825     C                   ENDSR
058600050825     C*------------------------------------------------------------------------*
058700080213
058800080213
058900080213
059000080213     C*------------------------------------------------------------------------*
059100080213     C* FORZADWN - Routine di forzatura elaborazione immediata download
059200080213     C*------------------------------------------------------------------------*
059300080213     C     FORZADWN      BEGSR
059400080213     C*
059500080213     C                   clear                   tis781ds
059600080213     C                   eval      §781tip = vgdTIP
059700080213     C                   eval      §781ksu = vgdKSU
059800080213     C                   eval      §781tsc = vgdTSC
059900080213     C                   eval      §781dat = vgdDAT
060000080213     C                   eval      §781pgm = vgdPGM
060100080905     C*
060200080905     C                   clear                   TIS781DFLO
060300080905     C                   movel     'P'           §781floela
060400080905     C                   movel     TIS781DFLO    §781flo
060500080213     C*
060600080213     C* Finita l'elaborazione chiamo pgm
060700080213     C                   CALL      'TIS781C1'
060800080213     C                   parm      *blanks       esito             1
060900080213     C                   parm                    tis781ds
061000080213     C                   parm      *blanks       vgdPRG           10
061100080213     C*
061200080213     C                   ENDSR
061300080213     C*------------------------------------------------------------------------*
061400001218
061500001221
061600001221
061700001218
061800001218     C*------------------------------------------------------------------------*
061900001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
062000010131     C*          - e dei pacchi ingombranti non dichiarati
062100001218     C*------------------------------------------------------------------------*
062200001218     C     REPPTIAS      BEGSR
062300001218     C*
062400060215     D* Prendo importo da assicurare da TITA4 t.r.=J
062500001218     C                   movel     'J'           KTRC
062600001218     C     KTA4          chain     tita430c
062700001218     C                   if        %found(tita430c)
062800001218     C                   movel     ta4NOT        DSBL4J
062900001221     C                   z-add     §B4IAS        vabIAS
063000001221     C                   movel     §B4VAS        vabVAS
063100001221     C                   movel     §B4XCO        vabXCO
063200001227     C                   movel     §B4BAU        vabSCL
063300010131     C                   if        §B4BAI = 'S'
063400010131     C                   movel     §B4BAI        vabFTM
063500010131     C                   setoff                                       55
063600010131     C                   endif
063700001218     C                   endif
063800001218     C*
063900001218     C                   ENDSR
064000001218     C*------------------------------------------------------------------------*
064100001218
064200001218
064300001218
064400001218
064500001218     C*------------------------------------------------------------------------*
064600001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
064700001218     C*------------------------------------------------------------------------*
064800001218     C     REPRMA        BEGSR
064900001218     C*
065000060215     D* Prendo importo da assicurare da TITA4 t.r.=A
065100001218     C                   movel     'A'           KTRC
065200001218     C     KTA4          chain     tita430c
065300001218     C                   if        %found(tita430c)
065400001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
065500001218     C                   endif
065600001218     C*
065700001218     C                   ENDSR
065800001218     C*------------------------------------------------------------------------*
065900001218
066000001218
066100001218
066200001218
066300001218     C*------------------------------------------------------------------------*
066400001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
066500001218     C*           ... e ai dati del mittente originale
066600001218     C*------------------------------------------------------------------------*
066700001218     C     REPDEST       BEGSR
066800001218     C*
066900001218     D* Prendo il tipo anagrafica = D
067000001218     C                   movel     'D'           KTRC
067100010131     C     KTAA          chain     titaa30c
067200001218     C                   if        %found(titaa30c)
067300001221     C                   movel     taaRSC        vabRD2
067400001218     C                   endif
067500001218     C*
067600001221     D* Prendo il tipo anagrafica = O
067700001218     C                   movel     'O'           KTRC
067800001218     C     KTAA          chain     titaa30c
067900001218     C                   if        %found(titaa30c)
068000001221     C                   movel     taaRSC        vabRMO
068100120326     C***                movel     taaCAP        vabCMO
068200120326     C***                movel     taaNAZ        vabNMO
068300001218     C                   endif
068400001222     C                   if        vabNMO = *blanks
068500001222     C                   move      '.'           vabNMO
068600001222     C                   endif
068700001218     C*
068800001218     C                   ENDSR
068900001218     C*------------------------------------------------------------------------*
069000001218
069100001218
069200001218
069300001218
069400001218     C*------------------------------------------------------------------------*
069500001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
069600001218     C*------------------------------------------------------------------------*
069700001218     C     REPCSB        BEGSR
069800001218     C*
069900001219     C     KCSB          chain     tncsb03l
070000001219     C                   if        %found(tncsb03l)
070100001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
070200001218     C                   else
070300001221     C                   movel     csbTPA        vabTIC
070400001221     C                   move      csbTPI        vabTIC
070500001221     C                   z-add     csbCAS        vabCAS
070600001221     C                   movel     csbVCA        vabVCA
070700001221     C                   movel     csbGCA        vabGCA
070800001218     C                   endif
070900001218     C                   endif
071000001218     C*
071100001218     C                   ENDSR
071200001218     C*------------------------------------------------------------------------*
071300001218
071400010605
071500010605
071600010605
071700010605     C*------------------------------------------------------------------------*
071800010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
071900010605     C*------------------------------------------------------------------------*
072000010605     C     REPIMP        BEGSR
072100010605     C*
072200010605     C* Come prima cosa verifico se trattasi di bolla POSTE
072300010605     C                   if        vabtsp = 'P'
072400010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
072500010605     C                   if        vabvas = 'EUR'
072600010605     C                   eval      vabvmd = vabias
072700010605     C                   eval(h)   wrklire = vabvmd * 1936,27
072800010605     C                   eval      vabias = wrklire
072900010605     C                   eval      vabvas = *blanks
073000010605     C                   endif
073100010605     C*
073200010605     C                   if        vabvca = 'EUR'
073300010605     C                   eval      vabqft = vabcas
073400010605     C                   eval(h)   wrklire = vabqft * 1936,27
073500010605     C                   eval      vabcas = wrklire
073600010605     C                   eval      vabvca = *blanks
073700010605     C                   endif
073800010605     C*
073900010605     C                   eval      vabxco = *blanks
074000010605     C                   endif
074100010605     C*
074200010605     C                   ENDSR
074300030704
074400030704
074500030704
074600030704
074700030704     C*------------------------------------------------------------------------*
074800030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
074900030704     C*------------------------------------------------------------------------*
075000030704     C     REPANT        BEGSR
075100030704     C*
075200050825     C                   clear                   ds_vabANT
075300030704     C                   if        %subst(tasGVA:1:1) = 'B'
075400151019     C                   eval      kar5TRD = 'BAN'
075500040531     C     KFIAR5        chain     fiar531c
075600040531     C                   if        %found(fiar531c)
075700030704     C                   movel     ar5UNI        DAR5BAN
075800030704     C                   movel     §ar5TBA       vabTBA
075900030704     C                   movel     §ar5NBA       vabNBA
076000030704     C                   movel     §ar5TB2       vabTB2
076100030704     C                   movel     §ar5NB2       vabNB2
076200030704     C                   endif
076300030704     C                   endif
076400050825     C                   eval      vabANT = ds_vabANT
076500030704     C*
076600030704     C                   ENDSR
076700030704     C*------------------------------------------------------------------------*
076800010605
076900010605
077000010605
077100001218
077200001218
077300001218     C*------------------------------------------------------------------------*
077400001218     C* CARTBL - Routine di caricamento dati tabellati
077500001218     C*------------------------------------------------------------------------*
077600001218     C     CARTBL        BEGSR
077700001218     C*
077800001221     C                   Z-ADD     0             I                 4 0
077900050825     C                   IF        FLGUNI = 'S'
078000010301     C                   ADD       1             I
078100050825     C                   MOVEL     CODCLI        KUNI(I)
078200010301     C                   ELSE
078300001221     C                   MOVEL     '3Q'          COD
078400001218     C     KTAB          CHAIN     TABEL                              31
078500001218     C     *IN31         DOWEQ     '0'
078600090518     C                   IF        TBLFLG = ' '
078700090518     C***                IF        TBLFLG = ' ' AND
078800090518     C***                          %subst(TBLKEY:8:1) = ' '
078900001222     C                   MOVEL     TBLUNI        DS3Q
079000050825     C     §3QCKS        IFEQ      CODCLI
079100001221     C                   ADD       1             I
079200001221     C                   MOVEL     TBLKEY        KUNI(I)
079300001221     C                   ENDIF
079400001218     C                   ENDIF
079500001218     C     KTAB          READE     TABEL                                  31
079600001218     C                   ENDDO
079700010301     C                   ENDIF
079800120326     c* carico schiera tipi bolla nn validi ai fini della spedizione
079900090217     c                   clear                   k
080000090217     c                   movel     'TB'          cod
080100090217     c     ktab          setll     tabel00f
080200090217     c     ktab          reade     tabel00f
080300090217     c                   dow       not %eof(tabel00f)
080400090217     c                   if        tblflg<>'*'
080500090217     C                   movel     tbluni        dstb
080600140702     C                   IF        §tbrbl='R'
080700090217     C                   ADD       1             K
080800090217     C                   MOVEL     TBLKEY        TBO(K)
080900090217     c                   endif
081000140702     C                   IF        §tbrbl='C' and FLGSICS <> 'S'
081100140702     C                   ADD       1             K
081200140702     C                   MOVEL     TBLKEY        TBO(K)
081300140702     c                   endif
081400090217     c                   endif
081500090217     c     ktab          reade     tabel00f
081600090217     c                   enddo
081700090217     c
081800001218     C*
081900001218     C                   ENDSR
082000001221     C*------------------------------------------------------------------------*
082100151019
082200151019
082300151019
082400151019     C     RTVAR5FAT     BEGSR
082500151019     C*
082600151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
082700151019     C                   SETOFF                                       7071
082800151019     C                   EVAL      kar5TRD = 'FAT'
082900151019     C                   CLEAR                   DAR5FAT
083000151019     C     KFIAR5        CHAIN     FIAR531C
083100151019     C                   IF        %found(FIAR531C)
083200151019     C                   MOVEL     AR5UNI        DAR5FAT
083300151019     C                   IF        §AR5PKTAS > *zeros
083400151019     C                   SETON                                        70
083500151019     C                   EVAL      tasPKF = §AR5PKTAS
083600151019     C                   ENDIF
083700151019     C                   IF        §AR5VLTAS > *zeros
083800151019     C                   SETON                                        71
083900160505     C                   EVAL      tasVLF = §AR5VLTAS
084000151019     C                   ENDIF
084100151019     C                   ENDIF
084200151019     C*
084300151019     C                   ENDSR
084400160208
084500160208
084600160208
084700160208     C     RTVNAS        BEGSR
084800160208     C*
084900160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
085000160208     C                   CLEAR                   DTA4A
085100160208     C*
085200160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
085300160208     C                   CALL      'UBTA400R'
085400160208     C                   PARM      *blanks       UBTA4IOPZ
085500160208     C                   PARM      *blanks       UBTA4ITLA
085600160208     C                   PARM      tasAAS        UBTA4IAAS
085700160208     C                   PARM      tasLNP        UBTA4ILNP
085800160208     C                   PARM      tasNRS        UBTA4INRS
085900160208     C                   PARM      tasNSP        UBTA4INSP
086000160208     C                   PARM      'A'           UBTA4ITRC
086100160208     C                   PARM                    UBTA4OERR
086200160208     C                   PARM                    UBTA4ODS
086300160208     C                   PARM                    UBTA4OLEN
086400160208     C                   PARM                    UBTA4ODATI
086500160208     C*
086600160208     C                   IF        UBTA4OERR = *zeros
086700160208     C                   SELECT
086800160208     C* Gestione output tipo record 'A'
086900160208     C                   WHEN      UBTA4ODS = 'DTA4A'
087000160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
087100160208     C                   ENDSL
087200160208     C*
087300160208     C                   ENDIF
087400160208     C*
087500160208     C                   ENDSR
087600001218
087700001218
087800001218
087900001218     C*------------------------------------------------------------------------*
088000001218     C* *INZSR - ROUTINE INIZIALE
088100001218     C*------------------------------------------------------------------------*
088200001218     C     *INZSR        BEGSR
088300001218     C*
088400001218     C     *ENTRY        PLIST
088500140702     C                   PARM                    PARAM            38
088600001218     C*
088700001218     C                   MOVEL     PARAM         DSINPUT
088800001218     C                   Z-ADD     1             CODUT
088900001218     C*
089000001218     C* Definizioni chiavi
089100010131     C     K38C          KLIST
089200010131     C                   KFLD                    depAAS            4 0
089300010131     C                   KFLD                    depMGS            4 0
089400010131     C*
089500001218     C     KTA4          KLIST
089600001218     C                   KFLD                    tasAAS
089700001218     C                   KFLD                    tasLNP
089800001218     C                   KFLD                    tasNRS
089900001218     C                   KFLD                    tasNSP
090000001218     C                   KFLD                    KTRC              1
090100001218     C*
090200001218     C     KTAA          KLIST
090300001218     C                   KFLD                    tasAAS
090400001218     C                   KFLD                    tasLNP
090500001218     C                   KFLD                    tasNRS
090600001218     C                   KFLD                    tasNSP
090700001218     C                   KFLD                    KTRC
090800001218     C*
090900001218     C     KCSB          KLIST
091000001218     C                   KFLD                    tasAAS
091100001218     C                   KFLD                    tasLNP
091200001218     C                   KFLD                    tasNRS
091300001218     C                   KFLD                    tasNSP
091400001218     C*
091500001218     C     KTAB          KLIST
091600001218     C                   KFLD                    CODUT             1 0
091700001218     C                   KFLD                    COD               2
091800030704     C*
091900030704     C     KFIAR5        KLIST
092000030704     C                   KFLD                    tasAAS
092100030704     C                   KFLD                    tasLNP
092200030704     C                   KFLD                    tasNRS
092300030704     C                   KFLD                    tasNSP
092400030704     C                   KFLD                    kar5TRD
092500001218     C*
092600001218     C* Determino la data corrente
092700100312     C                   Z-ADD     *zeros        DATCOR            8 0
092800100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
092900001218     C*
093000001218     C                   ENDSR
