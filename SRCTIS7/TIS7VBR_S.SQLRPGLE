000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
001500161205     FFNVAB00T  UF A E             DISK    USROPN COMMIT
001600001218     D*--------------------
001700001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001800001218     D*--------------------
001900090407     D KUNI            S              7  0 DIM(1000) DESCEND
002000100312     D*----------------------------------------------------------
002100090217     D TBO             S              2    DIM(20)                              TIPI BOLLA recupero
002200001218     D*--------------------
002300001218     D* DS ESTERNE
002400001218     D*--------------------
002500161205     D psds           sds
002600161205     D  procname         *PROC
002700100719     D TITAS00F      E DS
002800900517     D KPJBA         E DS
002900001221     D DS3Q          E DS
003000090217     D DSTB          E DS
003100000526     D DSBL4J        E DS
003200050825     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003300161206     D iDSVGD        E DS                  INZ EXTNAME(TIVGD00F)
003400160208     D DTA4A         E DS
003500001218     D*--------------------
003600001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003700001218     D*--------------------
003800050825     D DSINPUT         DS
003900050825     D  DATADA                        8  0
004000050825     D  DATAA                         8  0
004100050825     D  CODCLI                        7  0
004200050825     D  FLGIMI                        1
004300050825     D  FLGUNI                        1
004400050825     D  FLGOPZ                        1
004500080115     D  CODCLIVAS                     7  0
004600100312     D  TIPFILE                       2
004700080213     D  FLGEXE                        1
004800080213     D  TIPCLI                        1
004900140702     D  FLGSICS                       1
005000010131     D*--------------------
005100010131     D* DS DI SCOMPOSIZIONE DATE
005200010131     D*--------------------
005300010131     D                 DS
005400010131     D  AAAA                   1      4  0
005500010131     D  MMGG                   5      8  0
005600010131     D  DATAWRK                1      8  0
005700030704     D*--------------------
005800030704     D* DS DI RIDEFINIZIONE FIAR5 TIPO RECORD "BAN"
005900030704     D*--------------------
006000030704     D DAR5BAN       E DS
006100151019     D DAR5FAT       E DS
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
007700160208     D*--------------------
007800160208     D* DEFINIZIONI A PROCEDURE ESTERNE
007900160208     D*--------------------
008000160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008100001218
008200010605
008300010605
008400920812     C*---------------------------------------------------------------*
008500001218     C* MAIN
008600001218     C*---------------------------------------------------------------*
008700100719     C*
008800100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008900100719     C
009000100719     C/EXEC SQL
009100100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009200100719     C/END-EXEC
009300100719     C
009400100719     C*
009500001218     C                   exsr      cartbl
009600001218     C                   exsr      chkpar
009700161205     C                   exsr      chkproc
009800161205     C                   exsr      endela
009900001218     C*
010000001218     C                   seton                                        LR
010100001218
010200001218
010300001218
010400001218     C*------------------------------------------------------------------------*
010500001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
010600001218     C*------------------------------------------------------------------------*
010700001218     C     CHKPAR        BEGSR
010800001218     C*
010900001218     C* Verifico le date
011000001218     C                   if        DATAA  = *zeros
011100001218     C                   movel     *all'9'       DATAA
011200001218     C                   endif
011300050825     C*
011400050825     C* Verifico la modalità d scrittura output richiesta:
011500050825     C*   1 = crea membro "RIC" in file FNVAB00T
011600050825     C*   2 = scrivi record in file TIVGD00F
011700050825     C                   setoff                                       2526
011800050825     C                   if        FLGOPZ = '1'
011900050825     C                   seton                                        25
012000050825     C                   endif
012100050825     C                   if        FLGOPZ = '2'
012200050825     C                   seton                                        26
012300050825     C                   endif
012400001218     C*
012500001218     C                   ENDSR
012600001218     C*------------------------------------------------------------------------*
012700161205
012800161205
012900161205
013000161205     C*------------------------------------------------------------------------*
013100161205     C* CHKPROC - Routine verifica e scelta PROCEDIMENTO
013200161205     C*------------------------------------------------------------------------*
013300161205     C     CHKPROC       BEGSR
013400161205     C*
013500161205     C* Inizializzo variabili d wrk
013600161205     C                   movel     'N'           wProcedi          1
013700161205     C*
013800161205     C* Apro il file di output
013900161205     C                   if        *in25
014000161205     C                   open      fnvab00t
014100161205     C                   movel     'S'           wProcedi
014200161205     C                   endif
014300161205     C*
014400161205     C* Inizializzo la transazione
014500161205     C                   if        *in26
014600161205     C                   CALL(e)   'TIS7VASR1'
014700161205     C                   PARM      '1'           iOPZ              1
014800161205     C                   PARM                    iDSVGD
014900161205     C                   PARM      *blanks       oPRG             10
015000161205     C                   PARM                    oOK               1
015100161205     C*
015200161205     C* Verifico esito chiamata
015300161205     C                   if        %error OR oOK = *off
015400161205     C                   exsr      EXEERR
015500161205     C                   endif
015600161205     C*
015700161205     C* Se OK => proseguo
015800161206     C                   if        oOK = *on
015900161205     C                   movel     'S'           wProcedi
016000161206     C* Forzo momentaneamente il progressivo  a *blanks
016100161206     C                   eval      oPRG = *blanks
016200161205     C                   endif
016300161205     C                   endif
016400161205     C*
016500161205     C* Se ok a procedere => elaboro
016600161205     C                   if        wProcedi = 'S'
016700161205     C                   if        FLGIMI <> 'S'
016800161205     C                   exsr      procedi
016900161205     C                   else
017000161205     C                   exsr      procimi
017100161205     C                   endif
017200161205     C                   endif
017300161205     C*
017400161205     C* Chiudo il file di output
017500161205     C   25              close     fnvab00t
017600161205     C*
017700161205     C* Finalizzo la transazione
017800161205     C   26              CALL      'TIS7VASR1'
017900161205     C                   PARM      '3'           iOPZ
018000161205     C                   PARM                    iDSVGD
018100161205     C                   PARM                    oPRG
018200161205     C                   PARM                    oOK
018300161205     C*
018400161205     C* Verifico esito chiamata
018500161205     C                   if        %error OR oOK = *off
018600161205     C                   exsr      EXEERR
018700161205     C                   endif
018800161205     C*
018900161205     C                   ENDSR
019000161205     C*------------------------------------------------------------------------*
019100001218
019200001218
019300001218
019400001218     C*------------------------------------------------------------------------*
019500001218     C* PROCEDI - Routine principale
019600001218     C*------------------------------------------------------------------------*
019700001218     C     PROCEDI       BEGSR
019800001218     C*
019900001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
020000001218     C                   z-add     1             I
020100090407     C                   sorta     KUNI
020200001218     C                   dow       KUNI(I) > *zeros
020300100719     C*
020400100719     C* Se richiesta elaborazione x codice cliente mittente...
020500100719     C                   select
020600100719     C                   when      TIPCLI = *blanks
020700100719     C                   exsr      EXECCM
020800100719     C*
020900100719     C* Se richiesta elaborazione x codice cliente fatturazione...
021000100719     C                   when      TIPCLI = 'K'
021100100719     C                   exsr      EXEKSC
021200100719     C*
021300100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
021400100719     C                   when      TIPCLI = 'E'
021500100719     C                   exsr      EXECCM
021600100719     C                   exsr      EXEKSC
021700100719     C*
021800100719     C                   endsl
021900001218     C*
022000001218     C                   add       1             I
022100001218     C                   enddo
022200001218     C*
022300001218     C                   ENDSR
022400001218     C*------------------------------------------------------------------------*
022500100719
022600100719
022700100719
022800100719     C*------------------------------------------------------------------------*
022900100719     C* EXECCM   - Elaborazione x codice cliente mittente
023000100719     C*------------------------------------------------------------------------*
023100100719     C     EXECCM        BEGSR
023200100719     C*
023300100719     C                   movel     'C'           wChkCli           1
023400100719     C*
023500100719     C                   exsr      SQ_CCMDSP
023600100719     C*
023700100719     C                   ENDSR
023800100719     C*------------------------------------------------------------------------*
023900100719
024000100719
024100100719
024200100719     C*------------------------------------------------------------------------*
024300100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
024400100719     C*------------------------------------------------------------------------*
024500100719     C     EXEKSC        BEGSR
024600100719     C*
024700100719     C                   movel     'K'           wChkCli           1
024800100719     C*
024900100719     C                   exsr      SQ_KSCDSP
025000100719     C*
025100100719     C                   ENDSR
025200100719     C*------------------------------------------------------------------------*
025300100719
025400100719
025500100719
025600100719     C*------------------------------------------------------------------------*
025700100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025800100719     C*------------------------------------------------------------------------*
025900100719     C     SQ_CCMDSP     BEGSR
026000100719     C*
026100100719     C                   z-add     KUNI(I)       wParCCM           7 0
026200100719     C*
026300100719     C/EXEC SQL
026400100719     C+ declare C_CCMDSP cursor for
026500100719     C+ select * from titas00f
026600100719     C+ where tasccm = :wParCCM and
026700100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026800100719     C+ union
026900100719     C+ select * from titas10f
027000100719     C+ where tasccm = :wParCCM and
027100100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027200100719     C+ union
027300100719     C+ select * from titasP0f
027400100719     C+ where tasccm = :wParCCM and
027500100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027600100719     C+ for read only
027700100719     C/END-EXEC
027800100719     C
027900100719     C/EXEC SQL
028000100719     C+ open C_CCMDSP
028100100719     C/END-EXEC
028200100719     C
028300100719     C/EXEC SQL
028400100719     C+ Fetch C_CCMDSP into :TITAS00F
028500100719     C/END-EXEC
028600100719     C*
028700100719     C                   dow       sqlcod = *zeros
028800100719     C                   exsr      verifica
028900100719     C                   if        CHKREC = 'S'
029000100719     C                   exsr      scriviVAB
029100100719     C                   endif
029200100719     C
029300100719     C/EXEC SQL
029400100719     C+ Fetch C_CCMDSP into :TITAS00F
029500100719     C/END-EXEC
029600100719     C*
029700100719     C                   enddo
029800100719     C*
029900100719     C/EXEC SQL
030000100719     C+ close C_CCMDSP
030100100719     C/END-EXEC
030200100719     C*
030300100719     C*
030400100719     C                   ENDSR
030500100719     C*------------------------------------------------------------------------*
030600100719
030700100719
030800100719
030900100719     C*------------------------------------------------------------------------*
031000100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
031100100719     C*------------------------------------------------------------------------*
031200100719     C     SQ_KSCDSP     BEGSR
031300100719     C*
031400100719     C                   z-add     KUNI(I)       wParKSC           7 0
031500100719     C*
031600100719     C/EXEC SQL
031700100719     C+ declare C_KSCDSP cursor for
031800100719     C+ select * from titas00f
031900100719     C+ where tasksc = :wParKSC and
032000100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032100100719     C+ union
032200100719     C+ select * from titas10f
032300100719     C+ where tasksc = :wParKSC and
032400100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032500100719     C+ union
032600100719     C+ select * from titasP0f
032700100719     C+ where tasksc = :wParKSC and
032800100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032900100719     C+ for read only
033000100719     C/END-EXEC
033100100719     C
033200100719     C/EXEC SQL
033300100719     C+ open C_KSCDSP
033400100719     C/END-EXEC
033500100719     C
033600100719     C/EXEC SQL
033700100719     C+ Fetch C_KSCDSP into :TITAS00F
033800100719     C/END-EXEC
033900100719     C*
034000100719     C                   dow       sqlcod = *zeros
034100100719     C                   exsr      verifica
034200100719     C                   if        CHKREC = 'S'
034300100719     C                   exsr      scriviVAB
034400100719     C                   endif
034500100719     C
034600100719     C/EXEC SQL
034700100719     C+ Fetch C_KSCDSP into :TITAS00F
034800100719     C/END-EXEC
034900100719     C*
035000100719     C                   enddo
035100100719     C*
035200100719     C/EXEC SQL
035300100719     C+ close C_KSCDSP
035400100719     C/END-EXEC
035500100719     C*
035600100719     C*
035700100719     C                   ENDSR
035800100719     C*------------------------------------------------------------------------*
035900100719
036000001218
036100001218
036200010131     C*------------------------------------------------------------------------*
036300161205     C* PROCIMI - Routine principale per richiesta lancio IMI
036400010131     C*------------------------------------------------------------------------*
036500081006     C     PROCIMI       BEGSR
036600010131     C*
036700010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
036800010131     C                   z-add     DATADA        DATAWRK
036900010131     C                   z-add     AAAA          depAAS
037000010131     C                   z-add     MMGG          depMGS
037100010131     C*
037200010131     C     K38C          setll     titas38c
037300010131     C                   read      titas38c
037400010131     C*
037500010131     C                   dow       not %eof(titas38c)
037600010131     C* Verifico di non essere fuori range date
037700010131     C                   z-add     tasaas        AAAA
037800010131     C                   z-add     tasmgs        MMGG
037900010131     C                   if        DATAWRK > DATAA
038000010131     C                   leave
038100010131     C                   else
038200010131     C                   exsr      verifica
038300010131     C                   if        CHKREC = 'S'
038400010131     C                   exsr      scriviVAB
038500010131     C                   endif
038600010131     C                   endif
038700010131     C                   read      titas38c
038800010131     C                   enddo
038900010131     C*
039000010131     C                   ENDSR
039100010131     C*------------------------------------------------------------------------*
039200010131
039300010131
039400001218
039500001218     C*------------------------------------------------------------------------*
039600001218     C* VERIFICA - Routine di verifica validità record corrente
039700001218     C*------------------------------------------------------------------------*
039800001218     C     VERIFICA      BEGSR
039900001218     C*
040000001218     C                   movel     'S'           CHKREC            1
040100001218     C*
040200001218     C* Verifico le date
040300001218     C                   if        CHKREC = 'S'
040400001218     C                   movel     tasaas        wrkdata           8 0
040500001218     C                   move      tasmgs        wrkdata
040600001218     C                   if        wrkdata < DATADA or
040700001218     C                             wrkdata > DATAA
040800001218     C                   movel     'N'           CHKREC
040900001218     C                   endif
041000001218     C                   endif
041100001218     C*
041200010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
041300001218     C                   if        CHKREC = 'S'
041400010131     C                   if        FLGIMI <> 'S'
041500010301     C                   if        tasksc <> KUNI(I)
041600010301     C                   movel     'N'           CHKREC
041700001218     C                   endif
041800010131     C                   endif
041900001218     C                   endif
042000010131     C*
042100010131     C* Verifico esistenza IMI qualora richiesto nel lancio
042200010131     C                   if        CHKREC = 'S'
042300010131     C                   if        FLGIMI = 'S'
042400010131     C                   if        tasftc = 'F' or tastc2 = 'F'
042500010131     C                   else
042600010131     C                   movel     'N'           CHKREC
042700010131     C                   endif
042800010131     C                   endif
042900010131     C                   endif
043000100719     C*
043100100719     C* Verifico il codice cliente mittente
043200100719     C                   if        CHKREC = 'S'
043300100719     C                   if        wChkCli  = 'C'      AND
043400100719     C                             tasccm  <> KUNI(I)
043500100719     C                   movel     'N'           CHKREC
043600100719     C                   endif
043700100719     C                   endif
043800100719     C*
043900100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
044000100719     C                   if        CHKREC = 'S'
044100100719     C                   if        wChkCli  = 'K'      AND
044200100719     C                             tasksc  <> KUNI(I)
044300100719     C                   movel     'N'           CHKREC
044400100719     C                   endif
044500100719     C                   endif
044600100719     C*
044700100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
044800100719     C                   if        CHKREC = 'S'
044900100719     C                   if        TIPCLI  = 'E'       AND
045000100719     C                             wChkCli = 'K'       AND
045100100719     C                             tasksc  = tasccm
045200100719     C                   movel     'N'           CHKREC
045300100719     C                   endif
045400100719     C                   endif
045500140702     C*
045600090217     C* Verifico il tipo bolla che non deve essere di reupero
045700090217     C                   if        CHKREC = 'S'
045800090217     C                   if        %lookup(tastbl:tbo)>0
045900090217     C                   movel     'N'           CHKREC
046000090217     C                   endif
046100090217     C                   endif
046200001218     C*
046300001218     C                   ENDSR
046400001218     C*------------------------------------------------------------------------*
046500001218
046600001218
046700001218
046800001218     C*------------------------------------------------------------------------*
046900001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
047000001218     C*------------------------------------------------------------------------*
047100001221     C     SCRIVIVAB     BEGSR
047200001218     C*
047300001227     C                   clear                   fnvab000
047400161205     C                   if        FLGIMI = 'S'
047500010131     C                   seton                                        55
047600010131     C                   else
047700010131     C                   setoff                                       55
047800010131     C                   endif
047900151019     C*
048000151019     C* Eseguo eventuali "forzature"
048100151019     C                   EXSR      RTVAR5FAT
048200160208     C*
048300160208     C* Reprrisco NATURA MERCE
048400160208     C                   EXSR      RTVNAS
048500151019     C*
048600001221     C                   movel     tascbo        vabcbo
048700001221     C                   movel     tasrsd        vabrsd
048800001221     C                   movel     tasind        vabind
048900001221     C                   movel     tascad        vabcad
049000001221     C                   movel     taslod        vablod
049100001221     C                   movel     tasprd        vabprd
049200001221     C                   movel     tasnzd        vabnzd
049300001221     C                   movel     tasgc1        vabgc1
049400001221     C                   movel     tasgc2        vabgc2
049500001221     C                   movel     tastsp        vabtsp
049600160208     C                   movel     §TA4ANAS      vabnas
049700001221     C                   movel     tasctm        vabctm
049800001221     C                   movel     tasffd        vabffd
049900001221     C                   movel     tastcr        vabtcr
050000001221     C                   movel     tascts        vabcts
050100001221     C                   movel     tasftm        vabftm
050200001221     C                   movel     tasgma        vabgma
050300001221     C                   movel     tasgga        vabgga
050400001221     C                   movel     tasgva        vabgva
050500010111     C                   move      ' '           vabgva
050600100927     C                   movel     tasftc        vabtc1
050700100927     C                   movel     tastc2        vabtc2
050800001221     C                   if        tasccm > 0
050900001221     C                   z-add     tasccm        vabccm
051000001221     C                   else
051100001221     C                   z-add     tasksc        vabccm
051200001221     C                   end
051300001221     C                   z-add     taslnp        vablnp
051400001221     C                   z-add     tasaas        vabaas
051500001221     C                   z-add     tasmgs        vabmgs
051600001221     C                   z-add     tasnrs        vabnrs
051700001221     C                   z-add     tasnsp        vabnsp
051800001221     C                   z-add     taslna        vablna
051900001221     C                   z-add     tasctr        vabctr
052000001221     C                   z-add     tasncl        vabncl
052100001221     C                   z-add     taspkf        vabpkb
052200001221     C                   z-add     tasvlf        vabvlb
052300001221     C                   z-add     tasqft        vabqft
052400001221     C                   z-add     tasrmn        vabrmn
052500001221     C                   z-add     tasncd        vabncd
052600001221     C                   z-add     tasnca        vabnca
052700001221     C                   z-add     tasznc        vabznc
052800010108     C                   z-add     tasdbr        vabdcr
052900001221     C* Effettuo considerazioni particolare per bolla "POSTE"
053000001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
053100001227     C                   exsr      REPPTIAS
053200001227     C                   else
053300001227     C                   movel     tasIAS        vabIAS
053400001227     C                   movel     tasVAS        vabVAS
053500001227     C                   movel     tasXCO        vabXCO
053600001227     C                   endif
053700001227     C* Considerazioni per bolle "autogenerate"
053800001227     C                   if        tasSOP = '&' and
053900001227     C                             vabSCL = *blanks
054000001227     C                   movel     'A'           vabSCL
054100001227     C                   endif
054200001227     C*
054300001227     C                   exsr      REPCSB
054400010131     C                   exsr      REPRMA
054500081006     C                   exsr      REPANT
054600081006     C                   exsr      REPDEST
054700010605     C*
054800010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
054900010605     C                   exsr      REPIMP
055000001218     C*
055100010131     C                   if        *in55 = *off
055200050825     C   25              WRITE     FNVAB000
055300050825     C   26              exsr      WRITIVGD
055400010131     C                   endif
055500001218     C*
055600001218     C                   ENDSR
055700001221     C*------------------------------------------------------------------------*
055800050825
055900050825
056000050825
056100050825     C*------------------------------------------------------------------------*
056200050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
056300050825     C*------------------------------------------------------------------------*
056400050825     C     WRITIVGD      BEGSR
056500050825     C*
056600161205     C                   clear                   iDSVGD
056700080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
056800100312     C                   eval      vgdTIP = TIPFILE
056900050825     C                   movel     *all'0'       vgdKSU
057000080115     C                   move      CODCLIVAS     vgdKSU
057100050825     C                   eval      vgdTSC = 'WW'
057200161205     C                   eval      vgdDAT = datcor
057300161205     C                   eval      vgdPRG = oPRG
057400161205     C                   eval      vgdPGM = procname
057500161205     C*
057600161205     C                   CALL      'TIS7VASR1'
057700161205     C                   PARM      '2'           iOPZ
057800161205     C                   PARM                    iDSVGD
057900161205     C                   PARM                    oPRG
058000161205     C                   PARM                    oOK
058100161205     C*
058200161205     C* Verifico esito chiamata
058300161205     C                   if        %error OR oOK = *off
058400161205     C                   exsr      EXEERR
058500161205     C                   endif
058600050825     C*
058700050825     C                   ENDSR
058800050825     C*------------------------------------------------------------------------*
058900001221
059000001221
059100001218
059200001218     C*------------------------------------------------------------------------*
059300001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
059400010131     C*          - e dei pacchi ingombranti non dichiarati
059500001218     C*------------------------------------------------------------------------*
059600001218     C     REPPTIAS      BEGSR
059700001218     C*
059800060215     D* Prendo importo da assicurare da TITA4 t.r.=J
059900001218     C                   movel     'J'           KTRC
060000001218     C     KTA4          chain     tita430c
060100001218     C                   if        %found(tita430c)
060200001218     C                   movel     ta4NOT        DSBL4J
060300001221     C                   z-add     §B4IAS        vabIAS
060400001221     C                   movel     §B4VAS        vabVAS
060500001221     C                   movel     §B4XCO        vabXCO
060600001227     C                   movel     §B4BAU        vabSCL
060700010131     C                   if        §B4BAI = 'S'
060800010131     C                   movel     §B4BAI        vabFTM
060900010131     C                   setoff                                       55
061000010131     C                   endif
061100001218     C                   endif
061200001218     C*
061300001218     C                   ENDSR
061400001218     C*------------------------------------------------------------------------*
061500001218
061600001218
061700001218
061800001218     C*------------------------------------------------------------------------*
061900001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
062000001218     C*------------------------------------------------------------------------*
062100001218     C     REPRMA        BEGSR
062200001218     C*
062300060215     D* Prendo importo da assicurare da TITA4 t.r.=A
062400001218     C                   movel     'A'           KTRC
062500001218     C     KTA4          chain     tita430c
062600001218     C                   if        %found(tita430c)
062700001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
062800001218     C                   endif
062900001218     C*
063000001218     C                   ENDSR
063100001218     C*------------------------------------------------------------------------*
063200001218
063300001218
063400001218
063500001218     C*------------------------------------------------------------------------*
063600001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
063700001218     C*           ... e ai dati del mittente originale
063800001218     C*------------------------------------------------------------------------*
063900001218     C     REPDEST       BEGSR
064000001218     C*
064100001218     D* Prendo il tipo anagrafica = D
064200001218     C                   movel     'D'           KTRC
064300010131     C     KTAA          chain     titaa30c
064400001218     C                   if        %found(titaa30c)
064500001221     C                   movel     taaRSC        vabRD2
064600001218     C                   endif
064700001218     C*
064800001221     D* Prendo il tipo anagrafica = O
064900001218     C                   movel     'O'           KTRC
065000001218     C     KTAA          chain     titaa30c
065100001218     C                   if        %found(titaa30c)
065200001221     C                   movel     taaRSC        vabRMO
065300120326     C***                movel     taaCAP        vabCMO
065400120326     C***                movel     taaNAZ        vabNMO
065500001218     C                   endif
065600001222     C                   if        vabNMO = *blanks
065700001222     C                   move      '.'           vabNMO
065800001222     C                   endif
065900001218     C*
066000001218     C                   ENDSR
066100001218     C*------------------------------------------------------------------------*
066200001218
066300001218
066400001218
066500001218     C*------------------------------------------------------------------------*
066600001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
066700001218     C*------------------------------------------------------------------------*
066800001218     C     REPCSB        BEGSR
066900001218     C*
067000001219     C     KCSB          chain     tncsb03l
067100001219     C                   if        %found(tncsb03l)
067200001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
067300001218     C                   else
067400001221     C                   movel     csbTPA        vabTIC
067500001221     C                   move      csbTPI        vabTIC
067600001221     C                   z-add     csbCAS        vabCAS
067700001221     C                   movel     csbVCA        vabVCA
067800001221     C                   movel     csbGCA        vabGCA
067900001218     C                   endif
068000001218     C                   endif
068100001218     C*
068200001218     C                   ENDSR
068300001218     C*------------------------------------------------------------------------*
068400001218
068500010605
068600010605
068700010605     C*------------------------------------------------------------------------*
068800010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
068900010605     C*------------------------------------------------------------------------*
069000010605     C     REPIMP        BEGSR
069100010605     C*
069200010605     C* Come prima cosa verifico se trattasi di bolla POSTE
069300010605     C                   if        vabtsp = 'P'
069400010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
069500010605     C                   if        vabvas = 'EUR'
069600010605     C                   eval      vabvmd = vabias
069700010605     C                   eval(h)   wrklire = vabvmd * 1936,27
069800010605     C                   eval      vabias = wrklire
069900010605     C                   eval      vabvas = *blanks
070000010605     C                   endif
070100010605     C*
070200010605     C                   if        vabvca = 'EUR'
070300010605     C                   eval      vabqft = vabcas
070400010605     C                   eval(h)   wrklire = vabqft * 1936,27
070500010605     C                   eval      vabcas = wrklire
070600010605     C                   eval      vabvca = *blanks
070700010605     C                   endif
070800010605     C*
070900010605     C                   eval      vabxco = *blanks
071000010605     C                   endif
071100010605     C*
071200010605     C                   ENDSR
071300030704
071400030704
071500030704
071600030704     C*------------------------------------------------------------------------*
071700030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
071800030704     C*------------------------------------------------------------------------*
071900030704     C     REPANT        BEGSR
072000030704     C*
072100050825     C                   clear                   ds_vabANT
072200030704     C                   if        %subst(tasGVA:1:1) = 'B'
072300151019     C                   eval      kar5TRD = 'BAN'
072400040531     C     KFIAR5        chain     fiar531c
072500040531     C                   if        %found(fiar531c)
072600030704     C                   movel     ar5UNI        DAR5BAN
072700030704     C                   movel     §ar5TBA       vabTBA
072800030704     C                   movel     §ar5NBA       vabNBA
072900030704     C                   movel     §ar5TB2       vabTB2
073000030704     C                   movel     §ar5NB2       vabNB2
073100030704     C                   endif
073200030704     C                   endif
073300050825     C                   eval      vabANT = ds_vabANT
073400030704     C*
073500030704     C                   ENDSR
073600030704     C*------------------------------------------------------------------------*
073700010605
073800001218
073900001218
074000001218     C*------------------------------------------------------------------------*
074100001218     C* CARTBL - Routine di caricamento dati tabellati
074200001218     C*------------------------------------------------------------------------*
074300001218     C     CARTBL        BEGSR
074400001218     C*
074500001221     C                   Z-ADD     0             I                 4 0
074600050825     C                   IF        FLGUNI = 'S'
074700010301     C                   ADD       1             I
074800050825     C                   MOVEL     CODCLI        KUNI(I)
074900010301     C                   ELSE
075000001221     C                   MOVEL     '3Q'          COD
075100001218     C     KTAB          CHAIN     TABEL                              31
075200001218     C     *IN31         DOWEQ     '0'
075300090518     C                   IF        TBLFLG = ' '
075400090518     C***                IF        TBLFLG = ' ' AND
075500090518     C***                          %subst(TBLKEY:8:1) = ' '
075600001222     C                   MOVEL     TBLUNI        DS3Q
075700050825     C     §3QCKS        IFEQ      CODCLI
075800001221     C                   ADD       1             I
075900001221     C                   MOVEL     TBLKEY        KUNI(I)
076000001221     C                   ENDIF
076100001218     C                   ENDIF
076200001218     C     KTAB          READE     TABEL                                  31
076300001218     C                   ENDDO
076400010301     C                   ENDIF
076500120326     c* carico schiera tipi bolla nn validi ai fini della spedizione
076600090217     c                   clear                   k
076700090217     c                   movel     'TB'          cod
076800090217     c     ktab          setll     tabel00f
076900090217     c     ktab          reade     tabel00f
077000090217     c                   dow       not %eof(tabel00f)
077100090217     c                   if        tblflg<>'*'
077200090217     C                   movel     tbluni        dstb
077300140702     C                   IF        §tbrbl='R'
077400090217     C                   ADD       1             K
077500090217     C                   MOVEL     TBLKEY        TBO(K)
077600090217     c                   endif
077700140702     C                   IF        §tbrbl='C' and FLGSICS <> 'S'
077800140702     C                   ADD       1             K
077900140702     C                   MOVEL     TBLKEY        TBO(K)
078000140702     c                   endif
078100090217     c                   endif
078200090217     c     ktab          reade     tabel00f
078300090217     c                   enddo
078400090217     c
078500001218     C*
078600001218     C                   ENDSR
078700001221     C*------------------------------------------------------------------------*
078800151019
078900151019
079000151019
079100151019     C     RTVAR5FAT     BEGSR
079200151019     C*
079300151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
079400151019     C                   SETOFF                                       7071
079500151019     C                   EVAL      kar5TRD = 'FAT'
079600151019     C                   CLEAR                   DAR5FAT
079700151019     C     KFIAR5        CHAIN     FIAR531C
079800151019     C                   IF        %found(FIAR531C)
079900151019     C                   MOVEL     AR5UNI        DAR5FAT
080000151019     C                   IF        §AR5PKTAS > *zeros
080100151019     C                   SETON                                        70
080200151019     C                   EVAL      tasPKF = §AR5PKTAS
080300151019     C                   ENDIF
080400151019     C                   IF        §AR5VLTAS > *zeros
080500151019     C                   SETON                                        71
080600160505     C                   EVAL      tasVLF = §AR5VLTAS
080700151019     C                   ENDIF
080800151019     C                   ENDIF
080900151019     C*
081000151019     C                   ENDSR
081100160208
081200160208
081300160208
081400160208     C     RTVNAS        BEGSR
081500160208     C*
081600160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
081700160208     C                   CLEAR                   DTA4A
081800160208     C*
081900160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
082000160208     C                   CALL      'UBTA400R'
082100160208     C                   PARM      *blanks       UBTA4IOPZ
082200160208     C                   PARM      *blanks       UBTA4ITLA
082300160208     C                   PARM      tasAAS        UBTA4IAAS
082400160208     C                   PARM      tasLNP        UBTA4ILNP
082500160208     C                   PARM      tasNRS        UBTA4INRS
082600160208     C                   PARM      tasNSP        UBTA4INSP
082700160208     C                   PARM      'A'           UBTA4ITRC
082800160208     C                   PARM                    UBTA4OERR
082900160208     C                   PARM                    UBTA4ODS
083000160208     C                   PARM                    UBTA4OLEN
083100160208     C                   PARM                    UBTA4ODATI
083200160208     C*
083300160208     C                   IF        UBTA4OERR = *zeros
083400160208     C                   SELECT
083500160208     C* Gestione output tipo record 'A'
083600160208     C                   WHEN      UBTA4ODS = 'DTA4A'
083700160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
083800160208     C                   ENDSL
083900160208     C*
084000160208     C                   ENDIF
084100160208     C*
084200160208     C                   ENDSR
084300161205
084400161205
084500161205
084600161205     C*------------------------------------------------------------------------*
084700161205     C* ENDELA - Routine di lanci di chiusura finali
084800161205     C*------------------------------------------------------------------------*
084900161205     C     ENDELA        BEGSR
085000161205     C*
085100161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
085200161205     C                   CALL      'UBTA400R'
085300161205     C                   PARM                    UBTA4IOPZ
085400161205     C                   PARM      'C'           UBTA4ITLA
085500161205     C                   PARM                    UBTA4IAAS
085600161205     C                   PARM                    UBTA4ILNP
085700161205     C                   PARM                    UBTA4INRS
085800161205     C                   PARM                    UBTA4INSP
085900161205     C                   PARM                    UBTA4ITRC
086000161205     C                   PARM                    UBTA4OERR
086100161205     C                   PARM                    UBTA4ODS
086200161205     C                   PARM                    UBTA4OLEN
086300161205     C                   PARM                    UBTA4ODATI
086400161205     C*
086500161205     C                   ENDSR
086600161205     C*------------------------------------------------------------------------*
086700161205
086800161205
086900161205
087000161205     C*------------------------------------------------------------------------*
087100161205     C* EXEERR - Routine di esecuzione azioni su Errore
087200161205     C*------------------------------------------------------------------------*
087300161205     C     EXEERR        BEGSR
087400161205     C*
087500161205     C                   dump(A)
087600161205     C                   rolbk
087700161205     C                   seton                                        lr
087800161205     C                   return
087900161205     C*
088000161205     C                   ENDSR
088100161205     C*------------------------------------------------------------------------*
088200001218
088300001218
088400001218
088500001218     C*------------------------------------------------------------------------*
088600001218     C* *INZSR - ROUTINE INIZIALE
088700001218     C*------------------------------------------------------------------------*
088800001218     C     *INZSR        BEGSR
088900001218     C*
089000001218     C     *ENTRY        PLIST
089100140702     C                   PARM                    PARAM            38
089200001218     C*
089300001218     C                   MOVEL     PARAM         DSINPUT
089400001218     C                   Z-ADD     1             CODUT
089500001218     C*
089600001218     C* Definizioni chiavi
089700010131     C     K38C          KLIST
089800010131     C                   KFLD                    depAAS            4 0
089900010131     C                   KFLD                    depMGS            4 0
090000010131     C*
090100001218     C     KTA4          KLIST
090200001218     C                   KFLD                    tasAAS
090300001218     C                   KFLD                    tasLNP
090400001218     C                   KFLD                    tasNRS
090500001218     C                   KFLD                    tasNSP
090600001218     C                   KFLD                    KTRC              1
090700001218     C*
090800001218     C     KTAA          KLIST
090900001218     C                   KFLD                    tasAAS
091000001218     C                   KFLD                    tasLNP
091100001218     C                   KFLD                    tasNRS
091200001218     C                   KFLD                    tasNSP
091300001218     C                   KFLD                    KTRC
091400001218     C*
091500001218     C     KCSB          KLIST
091600001218     C                   KFLD                    tasAAS
091700001218     C                   KFLD                    tasLNP
091800001218     C                   KFLD                    tasNRS
091900001218     C                   KFLD                    tasNSP
092000001218     C*
092100001218     C     KTAB          KLIST
092200001218     C                   KFLD                    CODUT             1 0
092300001218     C                   KFLD                    COD               2
092400030704     C*
092500030704     C     KFIAR5        KLIST
092600030704     C                   KFLD                    tasAAS
092700030704     C                   KFLD                    tasLNP
092800030704     C                   KFLD                    tasNRS
092900030704     C                   KFLD                    tasNSP
093000030704     C                   KFLD                    kar5TRD
093100001218     C*
093200001218     C* Determino la data corrente
093300100312     C                   Z-ADD     *zeros        DATCOR            8 0
093400100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
093500001218     C*
093600001218     C                   ENDSR
