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
003300161205     D iDSVGD        E DS                  EXTNAME(TIVGD00F)
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
015800161205     C                   if        oOK = *on AND oPRG <> *blanks
015900161205     C                   movel     'S'           wProcedi
016000161205     C                   endif
016100161205     C                   endif
016200161205     C*
016300161205     C* Se ok a procedere => elaboro
016400161205     C                   if        wProcedi = 'S'
016500161205     C                   if        FLGIMI <> 'S'
016600161205     C                   exsr      procedi
016700161205     C                   else
016800161205     C                   exsr      procimi
016900161205     C                   endif
017000161205     C                   endif
017100161205     C*
017200161205     C* Chiudo il file di output
017300161205     C   25              close     fnvab00t
017400161205     C*
017500161205     C* Finalizzo la transazione
017600161205     C   26              CALL      'TIS7VASR1'
017700161205     C                   PARM      '3'           iOPZ
017800161205     C                   PARM                    iDSVGD
017900161205     C                   PARM                    oPRG
018000161205     C                   PARM                    oOK
018100161205     C*
018200161205     C* Verifico esito chiamata
018300161205     C                   if        %error OR oOK = *off
018400161205     C                   exsr      EXEERR
018500161205     C                   endif
018600161205     C*
018700161205     C                   ENDSR
018800161205     C*------------------------------------------------------------------------*
018900001218
019000001218
019100001218
019200001218     C*------------------------------------------------------------------------*
019300001218     C* PROCEDI - Routine principale
019400001218     C*------------------------------------------------------------------------*
019500001218     C     PROCEDI       BEGSR
019600001218     C*
019700001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
019800001218     C                   z-add     1             I
019900090407     C                   sorta     KUNI
020000001218     C                   dow       KUNI(I) > *zeros
020100100719     C*
020200100719     C* Se richiesta elaborazione x codice cliente mittente...
020300100719     C                   select
020400100719     C                   when      TIPCLI = *blanks
020500100719     C                   exsr      EXECCM
020600100719     C*
020700100719     C* Se richiesta elaborazione x codice cliente fatturazione...
020800100719     C                   when      TIPCLI = 'K'
020900100719     C                   exsr      EXEKSC
021000100719     C*
021100100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
021200100719     C                   when      TIPCLI = 'E'
021300100719     C                   exsr      EXECCM
021400100719     C                   exsr      EXEKSC
021500100719     C*
021600100719     C                   endsl
021700001218     C*
021800001218     C                   add       1             I
021900001218     C                   enddo
022000001218     C*
022100001218     C                   ENDSR
022200001218     C*------------------------------------------------------------------------*
022300100719
022400100719
022500100719
022600100719     C*------------------------------------------------------------------------*
022700100719     C* EXECCM   - Elaborazione x codice cliente mittente
022800100719     C*------------------------------------------------------------------------*
022900100719     C     EXECCM        BEGSR
023000100719     C*
023100100719     C                   movel     'C'           wChkCli           1
023200100719     C*
023300100719     C                   exsr      SQ_CCMDSP
023400100719     C*
023500100719     C                   ENDSR
023600100719     C*------------------------------------------------------------------------*
023700100719
023800100719
023900100719
024000100719     C*------------------------------------------------------------------------*
024100100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
024200100719     C*------------------------------------------------------------------------*
024300100719     C     EXEKSC        BEGSR
024400100719     C*
024500100719     C                   movel     'K'           wChkCli           1
024600100719     C*
024700100719     C                   exsr      SQ_KSCDSP
024800100719     C*
024900100719     C                   ENDSR
025000100719     C*------------------------------------------------------------------------*
025100100719
025200100719
025300100719
025400100719     C*------------------------------------------------------------------------*
025500100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
025600100719     C*------------------------------------------------------------------------*
025700100719     C     SQ_CCMDSP     BEGSR
025800100719     C*
025900100719     C                   z-add     KUNI(I)       wParCCM           7 0
026000100719     C*
026100100719     C/EXEC SQL
026200100719     C+ declare C_CCMDSP cursor for
026300100719     C+ select * from titas00f
026400100719     C+ where tasccm = :wParCCM and
026500100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
026600100719     C+ union
026700100719     C+ select * from titas10f
026800100719     C+ where tasccm = :wParCCM and
026900100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027000100719     C+ union
027100100719     C+ select * from titasP0f
027200100719     C+ where tasccm = :wParCCM and
027300100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
027400100719     C+ for read only
027500100719     C/END-EXEC
027600100719     C
027700100719     C/EXEC SQL
027800100719     C+ open C_CCMDSP
027900100719     C/END-EXEC
028000100719     C
028100100719     C/EXEC SQL
028200100719     C+ Fetch C_CCMDSP into :TITAS00F
028300100719     C/END-EXEC
028400100719     C*
028500100719     C                   dow       sqlcod = *zeros
028600100719     C                   exsr      verifica
028700100719     C                   if        CHKREC = 'S'
028800100719     C                   exsr      scriviVAB
028900100719     C                   endif
029000100719     C
029100100719     C/EXEC SQL
029200100719     C+ Fetch C_CCMDSP into :TITAS00F
029300100719     C/END-EXEC
029400100719     C*
029500100719     C                   enddo
029600100719     C*
029700100719     C/EXEC SQL
029800100719     C+ close C_CCMDSP
029900100719     C/END-EXEC
030000100719     C*
030100100719     C*
030200100719     C                   ENDSR
030300100719     C*------------------------------------------------------------------------*
030400100719
030500100719
030600100719
030700100719     C*------------------------------------------------------------------------*
030800100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
030900100719     C*------------------------------------------------------------------------*
031000100719     C     SQ_KSCDSP     BEGSR
031100100719     C*
031200100719     C                   z-add     KUNI(I)       wParKSC           7 0
031300100719     C*
031400100719     C/EXEC SQL
031500100719     C+ declare C_KSCDSP cursor for
031600100719     C+ select * from titas00f
031700100719     C+ where tasksc = :wParKSC and
031800100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
031900100719     C+ union
032000100719     C+ select * from titas10f
032100100719     C+ where tasksc = :wParKSC and
032200100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032300100719     C+ union
032400100719     C+ select * from titasP0f
032500100719     C+ where tasksc = :wParKSC and
032600100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
032700100719     C+ for read only
032800100719     C/END-EXEC
032900100719     C
033000100719     C/EXEC SQL
033100100719     C+ open C_KSCDSP
033200100719     C/END-EXEC
033300100719     C
033400100719     C/EXEC SQL
033500100719     C+ Fetch C_KSCDSP into :TITAS00F
033600100719     C/END-EXEC
033700100719     C*
033800100719     C                   dow       sqlcod = *zeros
033900100719     C                   exsr      verifica
034000100719     C                   if        CHKREC = 'S'
034100100719     C                   exsr      scriviVAB
034200100719     C                   endif
034300100719     C
034400100719     C/EXEC SQL
034500100719     C+ Fetch C_KSCDSP into :TITAS00F
034600100719     C/END-EXEC
034700100719     C*
034800100719     C                   enddo
034900100719     C*
035000100719     C/EXEC SQL
035100100719     C+ close C_KSCDSP
035200100719     C/END-EXEC
035300100719     C*
035400100719     C*
035500100719     C                   ENDSR
035600100719     C*------------------------------------------------------------------------*
035700100719
035800001218
035900001218
036000010131     C*------------------------------------------------------------------------*
036100161205     C* PROCIMI - Routine principale per richiesta lancio IMI
036200010131     C*------------------------------------------------------------------------*
036300081006     C     PROCIMI       BEGSR
036400010131     C*
036500010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
036600010131     C                   z-add     DATADA        DATAWRK
036700010131     C                   z-add     AAAA          depAAS
036800010131     C                   z-add     MMGG          depMGS
036900010131     C*
037000010131     C     K38C          setll     titas38c
037100010131     C                   read      titas38c
037200010131     C*
037300010131     C                   dow       not %eof(titas38c)
037400010131     C* Verifico di non essere fuori range date
037500010131     C                   z-add     tasaas        AAAA
037600010131     C                   z-add     tasmgs        MMGG
037700010131     C                   if        DATAWRK > DATAA
037800010131     C                   leave
037900010131     C                   else
038000010131     C                   exsr      verifica
038100010131     C                   if        CHKREC = 'S'
038200010131     C                   exsr      scriviVAB
038300010131     C                   endif
038400010131     C                   endif
038500010131     C                   read      titas38c
038600010131     C                   enddo
038700010131     C*
038800010131     C                   ENDSR
038900010131     C*------------------------------------------------------------------------*
039000010131
039100010131
039200001218
039300001218     C*------------------------------------------------------------------------*
039400001218     C* VERIFICA - Routine di verifica validità record corrente
039500001218     C*------------------------------------------------------------------------*
039600001218     C     VERIFICA      BEGSR
039700001218     C*
039800001218     C                   movel     'S'           CHKREC            1
039900001218     C*
040000001218     C* Verifico le date
040100001218     C                   if        CHKREC = 'S'
040200001218     C                   movel     tasaas        wrkdata           8 0
040300001218     C                   move      tasmgs        wrkdata
040400001218     C                   if        wrkdata < DATADA or
040500001218     C                             wrkdata > DATAA
040600001218     C                   movel     'N'           CHKREC
040700001218     C                   endif
040800001218     C                   endif
040900001218     C*
041000010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
041100001218     C                   if        CHKREC = 'S'
041200010131     C                   if        FLGIMI <> 'S'
041300010301     C                   if        tasksc <> KUNI(I)
041400010301     C                   movel     'N'           CHKREC
041500001218     C                   endif
041600010131     C                   endif
041700001218     C                   endif
041800010131     C*
041900010131     C* Verifico esistenza IMI qualora richiesto nel lancio
042000010131     C                   if        CHKREC = 'S'
042100010131     C                   if        FLGIMI = 'S'
042200010131     C                   if        tasftc = 'F' or tastc2 = 'F'
042300010131     C                   else
042400010131     C                   movel     'N'           CHKREC
042500010131     C                   endif
042600010131     C                   endif
042700010131     C                   endif
042800100719     C*
042900100719     C* Verifico il codice cliente mittente
043000100719     C                   if        CHKREC = 'S'
043100100719     C                   if        wChkCli  = 'C'      AND
043200100719     C                             tasccm  <> KUNI(I)
043300100719     C                   movel     'N'           CHKREC
043400100719     C                   endif
043500100719     C                   endif
043600100719     C*
043700100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
043800100719     C                   if        CHKREC = 'S'
043900100719     C                   if        wChkCli  = 'K'      AND
044000100719     C                             tasksc  <> KUNI(I)
044100100719     C                   movel     'N'           CHKREC
044200100719     C                   endif
044300100719     C                   endif
044400100719     C*
044500100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
044600100719     C                   if        CHKREC = 'S'
044700100719     C                   if        TIPCLI  = 'E'       AND
044800100719     C                             wChkCli = 'K'       AND
044900100719     C                             tasksc  = tasccm
045000100719     C                   movel     'N'           CHKREC
045100100719     C                   endif
045200100719     C                   endif
045300140702     C*
045400090217     C* Verifico il tipo bolla che non deve essere di reupero
045500090217     C                   if        CHKREC = 'S'
045600090217     C                   if        %lookup(tastbl:tbo)>0
045700090217     C                   movel     'N'           CHKREC
045800090217     C                   endif
045900090217     C                   endif
046000001218     C*
046100001218     C                   ENDSR
046200001218     C*------------------------------------------------------------------------*
046300001218
046400001218
046500001218
046600001218     C*------------------------------------------------------------------------*
046700001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
046800001218     C*------------------------------------------------------------------------*
046900001221     C     SCRIVIVAB     BEGSR
047000001218     C*
047100001227     C                   clear                   fnvab000
047200161205     C                   if        FLGIMI = 'S'
047300010131     C                   seton                                        55
047400010131     C                   else
047500010131     C                   setoff                                       55
047600010131     C                   endif
047700151019     C*
047800151019     C* Eseguo eventuali "forzature"
047900151019     C                   EXSR      RTVAR5FAT
048000160208     C*
048100160208     C* Reprrisco NATURA MERCE
048200160208     C                   EXSR      RTVNAS
048300151019     C*
048400001221     C                   movel     tascbo        vabcbo
048500001221     C                   movel     tasrsd        vabrsd
048600001221     C                   movel     tasind        vabind
048700001221     C                   movel     tascad        vabcad
048800001221     C                   movel     taslod        vablod
048900001221     C                   movel     tasprd        vabprd
049000001221     C                   movel     tasnzd        vabnzd
049100001221     C                   movel     tasgc1        vabgc1
049200001221     C                   movel     tasgc2        vabgc2
049300001221     C                   movel     tastsp        vabtsp
049400160208     C                   movel     §TA4ANAS      vabnas
049500001221     C                   movel     tasctm        vabctm
049600001221     C                   movel     tasffd        vabffd
049700001221     C                   movel     tastcr        vabtcr
049800001221     C                   movel     tascts        vabcts
049900001221     C                   movel     tasftm        vabftm
050000001221     C                   movel     tasgma        vabgma
050100001221     C                   movel     tasgga        vabgga
050200001221     C                   movel     tasgva        vabgva
050300010111     C                   move      ' '           vabgva
050400100927     C                   movel     tasftc        vabtc1
050500100927     C                   movel     tastc2        vabtc2
050600001221     C                   if        tasccm > 0
050700001221     C                   z-add     tasccm        vabccm
050800001221     C                   else
050900001221     C                   z-add     tasksc        vabccm
051000001221     C                   end
051100001221     C                   z-add     taslnp        vablnp
051200001221     C                   z-add     tasaas        vabaas
051300001221     C                   z-add     tasmgs        vabmgs
051400001221     C                   z-add     tasnrs        vabnrs
051500001221     C                   z-add     tasnsp        vabnsp
051600001221     C                   z-add     taslna        vablna
051700001221     C                   z-add     tasctr        vabctr
051800001221     C                   z-add     tasncl        vabncl
051900001221     C                   z-add     taspkf        vabpkb
052000001221     C                   z-add     tasvlf        vabvlb
052100001221     C                   z-add     tasqft        vabqft
052200001221     C                   z-add     tasrmn        vabrmn
052300001221     C                   z-add     tasncd        vabncd
052400001221     C                   z-add     tasnca        vabnca
052500001221     C                   z-add     tasznc        vabznc
052600010108     C                   z-add     tasdbr        vabdcr
052700001221     C* Effettuo considerazioni particolare per bolla "POSTE"
052800001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
052900001227     C                   exsr      REPPTIAS
053000001227     C                   else
053100001227     C                   movel     tasIAS        vabIAS
053200001227     C                   movel     tasVAS        vabVAS
053300001227     C                   movel     tasXCO        vabXCO
053400001227     C                   endif
053500001227     C* Considerazioni per bolle "autogenerate"
053600001227     C                   if        tasSOP = '&' and
053700001227     C                             vabSCL = *blanks
053800001227     C                   movel     'A'           vabSCL
053900001227     C                   endif
054000001227     C*
054100001227     C                   exsr      REPCSB
054200010131     C                   exsr      REPRMA
054300081006     C                   exsr      REPANT
054400081006     C                   exsr      REPDEST
054500010605     C*
054600010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
054700010605     C                   exsr      REPIMP
054800001218     C*
054900010131     C                   if        *in55 = *off
055000050825     C   25              WRITE     FNVAB000
055100050825     C   26              exsr      WRITIVGD
055200010131     C                   endif
055300001218     C*
055400001218     C                   ENDSR
055500001221     C*------------------------------------------------------------------------*
055600050825
055700050825
055800050825
055900050825     C*------------------------------------------------------------------------*
056000050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
056100050825     C*------------------------------------------------------------------------*
056200050825     C     WRITIVGD      BEGSR
056300050825     C*
056400161205     C                   clear                   iDSVGD
056500080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
056600100312     C                   eval      vgdTIP = TIPFILE
056700050825     C                   movel     *all'0'       vgdKSU
056800080115     C                   move      CODCLIVAS     vgdKSU
056900050825     C                   eval      vgdTSC = 'WW'
057000161205     C                   eval      vgdDAT = datcor
057100161205     C                   eval      vgdPRG = oPRG
057200161205     C                   eval      vgdPGM = procname
057300161205     C*
057400161205     C                   CALL      'TIS7VASR1'
057500161205     C                   PARM      '2'           iOPZ
057600161205     C                   PARM                    iDSVGD
057700161205     C                   PARM                    oPRG
057800161205     C                   PARM                    oOK
057900161205     C*
058000161205     C* Verifico esito chiamata
058100161205     C                   if        %error OR oOK = *off
058200161205     C                   exsr      EXEERR
058300161205     C                   endif
058400050825     C*
058500050825     C                   ENDSR
058600050825     C*------------------------------------------------------------------------*
058700001221
058800001221
058900001218
059000001218     C*------------------------------------------------------------------------*
059100001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
059200010131     C*          - e dei pacchi ingombranti non dichiarati
059300001218     C*------------------------------------------------------------------------*
059400001218     C     REPPTIAS      BEGSR
059500001218     C*
059600060215     D* Prendo importo da assicurare da TITA4 t.r.=J
059700001218     C                   movel     'J'           KTRC
059800001218     C     KTA4          chain     tita430c
059900001218     C                   if        %found(tita430c)
060000001218     C                   movel     ta4NOT        DSBL4J
060100001221     C                   z-add     §B4IAS        vabIAS
060200001221     C                   movel     §B4VAS        vabVAS
060300001221     C                   movel     §B4XCO        vabXCO
060400001227     C                   movel     §B4BAU        vabSCL
060500010131     C                   if        §B4BAI = 'S'
060600010131     C                   movel     §B4BAI        vabFTM
060700010131     C                   setoff                                       55
060800010131     C                   endif
060900001218     C                   endif
061000001218     C*
061100001218     C                   ENDSR
061200001218     C*------------------------------------------------------------------------*
061300001218
061400001218
061500001218
061600001218     C*------------------------------------------------------------------------*
061700001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
061800001218     C*------------------------------------------------------------------------*
061900001218     C     REPRMA        BEGSR
062000001218     C*
062100060215     D* Prendo importo da assicurare da TITA4 t.r.=A
062200001218     C                   movel     'A'           KTRC
062300001218     C     KTA4          chain     tita430c
062400001218     C                   if        %found(tita430c)
062500001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
062600001218     C                   endif
062700001218     C*
062800001218     C                   ENDSR
062900001218     C*------------------------------------------------------------------------*
063000001218
063100001218
063200001218
063300001218     C*------------------------------------------------------------------------*
063400001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
063500001218     C*           ... e ai dati del mittente originale
063600001218     C*------------------------------------------------------------------------*
063700001218     C     REPDEST       BEGSR
063800001218     C*
063900001218     D* Prendo il tipo anagrafica = D
064000001218     C                   movel     'D'           KTRC
064100010131     C     KTAA          chain     titaa30c
064200001218     C                   if        %found(titaa30c)
064300001221     C                   movel     taaRSC        vabRD2
064400001218     C                   endif
064500001218     C*
064600001221     D* Prendo il tipo anagrafica = O
064700001218     C                   movel     'O'           KTRC
064800001218     C     KTAA          chain     titaa30c
064900001218     C                   if        %found(titaa30c)
065000001221     C                   movel     taaRSC        vabRMO
065100120326     C***                movel     taaCAP        vabCMO
065200120326     C***                movel     taaNAZ        vabNMO
065300001218     C                   endif
065400001222     C                   if        vabNMO = *blanks
065500001222     C                   move      '.'           vabNMO
065600001222     C                   endif
065700001218     C*
065800001218     C                   ENDSR
065900001218     C*------------------------------------------------------------------------*
066000001218
066100001218
066200001218
066300001218     C*------------------------------------------------------------------------*
066400001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
066500001218     C*------------------------------------------------------------------------*
066600001218     C     REPCSB        BEGSR
066700001218     C*
066800001219     C     KCSB          chain     tncsb03l
066900001219     C                   if        %found(tncsb03l)
067000001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
067100001218     C                   else
067200001221     C                   movel     csbTPA        vabTIC
067300001221     C                   move      csbTPI        vabTIC
067400001221     C                   z-add     csbCAS        vabCAS
067500001221     C                   movel     csbVCA        vabVCA
067600001221     C                   movel     csbGCA        vabGCA
067700001218     C                   endif
067800001218     C                   endif
067900001218     C*
068000001218     C                   ENDSR
068100001218     C*------------------------------------------------------------------------*
068200001218
068300010605
068400010605
068500010605     C*------------------------------------------------------------------------*
068600010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
068700010605     C*------------------------------------------------------------------------*
068800010605     C     REPIMP        BEGSR
068900010605     C*
069000010605     C* Come prima cosa verifico se trattasi di bolla POSTE
069100010605     C                   if        vabtsp = 'P'
069200010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
069300010605     C                   if        vabvas = 'EUR'
069400010605     C                   eval      vabvmd = vabias
069500010605     C                   eval(h)   wrklire = vabvmd * 1936,27
069600010605     C                   eval      vabias = wrklire
069700010605     C                   eval      vabvas = *blanks
069800010605     C                   endif
069900010605     C*
070000010605     C                   if        vabvca = 'EUR'
070100010605     C                   eval      vabqft = vabcas
070200010605     C                   eval(h)   wrklire = vabqft * 1936,27
070300010605     C                   eval      vabcas = wrklire
070400010605     C                   eval      vabvca = *blanks
070500010605     C                   endif
070600010605     C*
070700010605     C                   eval      vabxco = *blanks
070800010605     C                   endif
070900010605     C*
071000010605     C                   ENDSR
071100030704
071200030704
071300030704
071400030704     C*------------------------------------------------------------------------*
071500030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
071600030704     C*------------------------------------------------------------------------*
071700030704     C     REPANT        BEGSR
071800030704     C*
071900050825     C                   clear                   ds_vabANT
072000030704     C                   if        %subst(tasGVA:1:1) = 'B'
072100151019     C                   eval      kar5TRD = 'BAN'
072200040531     C     KFIAR5        chain     fiar531c
072300040531     C                   if        %found(fiar531c)
072400030704     C                   movel     ar5UNI        DAR5BAN
072500030704     C                   movel     §ar5TBA       vabTBA
072600030704     C                   movel     §ar5NBA       vabNBA
072700030704     C                   movel     §ar5TB2       vabTB2
072800030704     C                   movel     §ar5NB2       vabNB2
072900030704     C                   endif
073000030704     C                   endif
073100050825     C                   eval      vabANT = ds_vabANT
073200030704     C*
073300030704     C                   ENDSR
073400030704     C*------------------------------------------------------------------------*
073500010605
073600001218
073700001218
073800001218     C*------------------------------------------------------------------------*
073900001218     C* CARTBL - Routine di caricamento dati tabellati
074000001218     C*------------------------------------------------------------------------*
074100001218     C     CARTBL        BEGSR
074200001218     C*
074300001221     C                   Z-ADD     0             I                 4 0
074400050825     C                   IF        FLGUNI = 'S'
074500010301     C                   ADD       1             I
074600050825     C                   MOVEL     CODCLI        KUNI(I)
074700010301     C                   ELSE
074800001221     C                   MOVEL     '3Q'          COD
074900001218     C     KTAB          CHAIN     TABEL                              31
075000001218     C     *IN31         DOWEQ     '0'
075100090518     C                   IF        TBLFLG = ' '
075200090518     C***                IF        TBLFLG = ' ' AND
075300090518     C***                          %subst(TBLKEY:8:1) = ' '
075400001222     C                   MOVEL     TBLUNI        DS3Q
075500050825     C     §3QCKS        IFEQ      CODCLI
075600001221     C                   ADD       1             I
075700001221     C                   MOVEL     TBLKEY        KUNI(I)
075800001221     C                   ENDIF
075900001218     C                   ENDIF
076000001218     C     KTAB          READE     TABEL                                  31
076100001218     C                   ENDDO
076200010301     C                   ENDIF
076300120326     c* carico schiera tipi bolla nn validi ai fini della spedizione
076400090217     c                   clear                   k
076500090217     c                   movel     'TB'          cod
076600090217     c     ktab          setll     tabel00f
076700090217     c     ktab          reade     tabel00f
076800090217     c                   dow       not %eof(tabel00f)
076900090217     c                   if        tblflg<>'*'
077000090217     C                   movel     tbluni        dstb
077100140702     C                   IF        §tbrbl='R'
077200090217     C                   ADD       1             K
077300090217     C                   MOVEL     TBLKEY        TBO(K)
077400090217     c                   endif
077500140702     C                   IF        §tbrbl='C' and FLGSICS <> 'S'
077600140702     C                   ADD       1             K
077700140702     C                   MOVEL     TBLKEY        TBO(K)
077800140702     c                   endif
077900090217     c                   endif
078000090217     c     ktab          reade     tabel00f
078100090217     c                   enddo
078200090217     c
078300001218     C*
078400001218     C                   ENDSR
078500001221     C*------------------------------------------------------------------------*
078600151019
078700151019
078800151019
078900151019     C     RTVAR5FAT     BEGSR
079000151019     C*
079100151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
079200151019     C                   SETOFF                                       7071
079300151019     C                   EVAL      kar5TRD = 'FAT'
079400151019     C                   CLEAR                   DAR5FAT
079500151019     C     KFIAR5        CHAIN     FIAR531C
079600151019     C                   IF        %found(FIAR531C)
079700151019     C                   MOVEL     AR5UNI        DAR5FAT
079800151019     C                   IF        §AR5PKTAS > *zeros
079900151019     C                   SETON                                        70
080000151019     C                   EVAL      tasPKF = §AR5PKTAS
080100151019     C                   ENDIF
080200151019     C                   IF        §AR5VLTAS > *zeros
080300151019     C                   SETON                                        71
080400160505     C                   EVAL      tasVLF = §AR5VLTAS
080500151019     C                   ENDIF
080600151019     C                   ENDIF
080700151019     C*
080800151019     C                   ENDSR
080900160208
081000160208
081100160208
081200160208     C     RTVNAS        BEGSR
081300160208     C*
081400160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
081500160208     C                   CLEAR                   DTA4A
081600160208     C*
081700160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
081800160208     C                   CALL      'UBTA400R'
081900160208     C                   PARM      *blanks       UBTA4IOPZ
082000160208     C                   PARM      *blanks       UBTA4ITLA
082100160208     C                   PARM      tasAAS        UBTA4IAAS
082200160208     C                   PARM      tasLNP        UBTA4ILNP
082300160208     C                   PARM      tasNRS        UBTA4INRS
082400160208     C                   PARM      tasNSP        UBTA4INSP
082500160208     C                   PARM      'A'           UBTA4ITRC
082600160208     C                   PARM                    UBTA4OERR
082700160208     C                   PARM                    UBTA4ODS
082800160208     C                   PARM                    UBTA4OLEN
082900160208     C                   PARM                    UBTA4ODATI
083000160208     C*
083100160208     C                   IF        UBTA4OERR = *zeros
083200160208     C                   SELECT
083300160208     C* Gestione output tipo record 'A'
083400160208     C                   WHEN      UBTA4ODS = 'DTA4A'
083500160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
083600160208     C                   ENDSL
083700160208     C*
083800160208     C                   ENDIF
083900160208     C*
084000160208     C                   ENDSR
084100161205
084200161205
084300161205
084400161205     C*------------------------------------------------------------------------*
084500161205     C* ENDELA - Routine di lanci di chiusura finali
084600161205     C*------------------------------------------------------------------------*
084700161205     C     ENDELA        BEGSR
084800161205     C*
084900161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
085000161205     C                   CALL      'UBTA400R'
085100161205     C                   PARM                    UBTA4IOPZ
085200161205     C                   PARM      'C'           UBTA4ITLA
085300161205     C                   PARM                    UBTA4IAAS
085400161205     C                   PARM                    UBTA4ILNP
085500161205     C                   PARM                    UBTA4INRS
085600161205     C                   PARM                    UBTA4INSP
085700161205     C                   PARM                    UBTA4ITRC
085800161205     C                   PARM                    UBTA4OERR
085900161205     C                   PARM                    UBTA4ODS
086000161205     C                   PARM                    UBTA4OLEN
086100161205     C                   PARM                    UBTA4ODATI
086200161205     C*
086300161205     C                   ENDSR
086400161205     C*------------------------------------------------------------------------*
086500161205
086600161205
086700161205
086800161205     C*------------------------------------------------------------------------*
086900161205     C* EXEERR - Routine di esecuzione azioni su Errore
087000161205     C*------------------------------------------------------------------------*
087100161205     C     EXEERR        BEGSR
087200161205     C*
087300161205     C                   dump(A)
087400161205     C                   rolbk
087500161205     C                   seton                                        lr
087600161205     C                   return
087700161205     C*
087800161205     C                   ENDSR
087900161205     C*------------------------------------------------------------------------*
088000001218
088100001218
088200001218
088300001218     C*------------------------------------------------------------------------*
088400001218     C* *INZSR - ROUTINE INIZIALE
088500001218     C*------------------------------------------------------------------------*
088600001218     C     *INZSR        BEGSR
088700001218     C*
088800001218     C     *ENTRY        PLIST
088900140702     C                   PARM                    PARAM            38
089000001218     C*
089100001218     C                   MOVEL     PARAM         DSINPUT
089200001218     C                   Z-ADD     1             CODUT
089300001218     C*
089400001218     C* Definizioni chiavi
089500010131     C     K38C          KLIST
089600010131     C                   KFLD                    depAAS            4 0
089700010131     C                   KFLD                    depMGS            4 0
089800010131     C*
089900001218     C     KTA4          KLIST
090000001218     C                   KFLD                    tasAAS
090100001218     C                   KFLD                    tasLNP
090200001218     C                   KFLD                    tasNRS
090300001218     C                   KFLD                    tasNSP
090400001218     C                   KFLD                    KTRC              1
090500001218     C*
090600001218     C     KTAA          KLIST
090700001218     C                   KFLD                    tasAAS
090800001218     C                   KFLD                    tasLNP
090900001218     C                   KFLD                    tasNRS
091000001218     C                   KFLD                    tasNSP
091100001218     C                   KFLD                    KTRC
091200001218     C*
091300001218     C     KCSB          KLIST
091400001218     C                   KFLD                    tasAAS
091500001218     C                   KFLD                    tasLNP
091600001218     C                   KFLD                    tasNRS
091700001218     C                   KFLD                    tasNSP
091800001218     C*
091900001218     C     KTAB          KLIST
092000001218     C                   KFLD                    CODUT             1 0
092100001218     C                   KFLD                    COD               2
092200030704     C*
092300030704     C     KFIAR5        KLIST
092400030704     C                   KFLD                    tasAAS
092500030704     C                   KFLD                    tasLNP
092600030704     C                   KFLD                    tasNRS
092700030704     C                   KFLD                    tasNSP
092800030704     C                   KFLD                    kar5TRD
092900001218     C*
093000001218     C* Determino la data corrente
093100100312     C                   Z-ADD     *zeros        DATCOR            8 0
093200100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
093300001218     C*
093400001218     C                   ENDSR
