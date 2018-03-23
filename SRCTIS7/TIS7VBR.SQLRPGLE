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
001600161206     FTIVGD00F  O    E             DISK    USROPN COMMIT
001700001218     D*--------------------
001800001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001900001218     D*--------------------
002000090407     D KUNI            S              7  0 DIM(1000) DESCEND
002100100312     D*----------------------------------------------------------
002200090217     D TBO             S              2    DIM(20)                              TIPI BOLLA recupero
002300001218     D*--------------------
002400001218     D* DS ESTERNE
002500001218     D*--------------------
002600161205     D psds           sds
002700161205     D  procname         *PROC
002800100719     D TITAS00F      E DS
002900900517     D KPJBA         E DS
003000001221     D DS3Q          E DS
003100090217     D DSTB          E DS
003200000526     D DSBL4J        E DS
003300050825     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003400160208     D DTA4A         E DS
003500170227     D TIS7VASDS     E DS
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
007800160208     D*--------------------
007900160208     D* DEFINIZIONI A PROCEDURE ESTERNE
008000160208     D*--------------------
008100160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008200001218
008300010605
008400010605
008500920812     C*---------------------------------------------------------------*
008600001218     C* MAIN
008700001218     C*---------------------------------------------------------------*
008800100719     C*
008900100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009000100719     C
009100100719     C/EXEC SQL
009200100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009300100719     C/END-EXEC
009400100719     C
009500161206     C*
009600161206     C* Avvio il monitoring del processo
009700161206     C                   monitor
009800161206     C*
009900001218     C                   exsr      cartbl
010000001218     C                   exsr      chkpar
010100161205     C                   exsr      chkproc
010200161205     C                   exsr      endela
010300161206     C*
010400161206     C* Sabcisco il commit
010500170309     C                   commit(e)
010600161206     C*
010700161206     C* Su errore
010800161206     C                   on-error
010900161206     C                   exsr      exeerr
011000161206     C*
011100161206     C* Fine monitoring
011200161206     C                   endmon
011300001218     C*
011400001218     C                   seton                                        LR
011500001218
011600001218
011700001218
011800001218     C*------------------------------------------------------------------------*
011900001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
012000001218     C*------------------------------------------------------------------------*
012100001218     C     CHKPAR        BEGSR
012200001218     C*
012300001218     C* Verifico le date
012400001218     C                   if        DATAA  = *zeros
012500001218     C                   movel     *all'9'       DATAA
012600001218     C                   endif
012700050825     C*
012800050825     C* Verifico la modalità d scrittura output richiesta:
012900050825     C*   1 = crea membro "RIC" in file FNVAB00T
013000050825     C*   2 = scrivi record in file TIVGD00F
013100050825     C                   setoff                                       2526
013200050825     C                   if        FLGOPZ = '1'
013300050825     C                   seton                                        25
013400050825     C                   endif
013500050825     C                   if        FLGOPZ = '2'
013600050825     C                   seton                                        26
013700050825     C                   endif
013800001218     C*
013900001218     C                   ENDSR
014000001218     C*------------------------------------------------------------------------*
014100161205
014200161205
014300161205
014400161205     C*------------------------------------------------------------------------*
014500161205     C* CHKPROC - Routine verifica e scelta PROCEDIMENTO
014600161205     C*------------------------------------------------------------------------*
014700161205     C     CHKPROC       BEGSR
014800161205     C*
014900161205     C* Inizializzo variabili d wrk
015000161205     C                   movel     'N'           wProcedi          1
015100161205     C*
015200161205     C* Apro il file di output
015300161205     C                   if        *in25
015400161205     C                   open      fnvab00t
015500161205     C                   movel     'S'           wProcedi
015600161205     C                   endif
015700161205     C*
015800161205     C* Inizializzo la transazione
015900161205     C                   if        *in26
016000161206     C                   open      tivgd00f
016100170227     C*
016200170227     C* Stacco progressivo univoco download
016300170227     C                   CLEAR                   TIS7VASDS
016400170227     C                   EVAL      i§7VASOPZ = 'PRG'
016500170227     C                   CALL(e)   'TIS7VASR1'
016600170227     C                   PARM                    TIS7VASDS
016700170227     C*
016800170227     C* Se OK => proseguo
016900170227     C                   if        not %error AND
017000170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
017100170227     C                   movel     'S'           wProcedi
017200170227     C                   endif
017300161206     C                   endif
017400161205     C*
017500161205     C* Se ok a procedere => elaboro
017600161205     C                   if        wProcedi = 'S'
017700161205     C                   if        FLGIMI <> 'S'
017800161205     C                   exsr      procedi
017900161205     C                   else
018000161205     C                   exsr      procimi
018100161205     C                   endif
018200161205     C                   endif
018300161205     C*
018400161205     C* Chiudo il file di output
018500161205     C   25              close     fnvab00t
018600161206     C   26              close     tivgd00f
018700170227     C*
018800170227     C* Finalizzo la transazione
018900170227     C                   if        *in26
019000170227     C                   EVAL      i§7VASOPZ  = 'RLS'
019100170227     C                   EVAL      i§7VASTIP  = TIPFILE
019200170227     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLIVAS:'X')
019300170227     C                   EVAL      i§7VASTSC  = 'WW'
019400170227     C                   EVAL      i§7VASSTO  = '?'
019500170227     C                   EVAL      i§7VASSTTO = 'L'
019600170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
019700170227     C                   CALL(e)   'TIS7VASR1'
019800170227     C                   PARM                    TIS7VASDS
019900170227     C*
020000170227     C* Verifico esito chiamata
020100170227     C                   if        %error OR o§7VASOK = *off
020200170227     C                   exsr      EXEERR
020300170227     C                   endif
020400170227     C                   endif
020500161205     C*
020600161205     C                   ENDSR
020700161205     C*------------------------------------------------------------------------*
020800001218
020900001218
021000001218
021100001218     C*------------------------------------------------------------------------*
021200001218     C* PROCEDI - Routine principale
021300001218     C*------------------------------------------------------------------------*
021400001218     C     PROCEDI       BEGSR
021500001218     C*
021600001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
021700001218     C                   z-add     1             I
021800090407     C                   sorta     KUNI
021900001218     C                   dow       KUNI(I) > *zeros
022000100719     C*
022100100719     C* Se richiesta elaborazione x codice cliente mittente...
022200100719     C                   select
022300100719     C                   when      TIPCLI = *blanks
022400100719     C                   exsr      EXECCM
022500100719     C*
022600100719     C* Se richiesta elaborazione x codice cliente fatturazione...
022700100719     C                   when      TIPCLI = 'K'
022800100719     C                   exsr      EXEKSC
022900100719     C*
023000100719     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
023100100719     C                   when      TIPCLI = 'E'
023200100719     C                   exsr      EXECCM
023300100719     C                   exsr      EXEKSC
023400100719     C*
023500100719     C                   endsl
023600001218     C*
023700001218     C                   add       1             I
023800001218     C                   enddo
023900001218     C*
024000001218     C                   ENDSR
024100001218     C*------------------------------------------------------------------------*
024200100719
024300100719
024400100719
024500100719     C*------------------------------------------------------------------------*
024600100719     C* EXECCM   - Elaborazione x codice cliente mittente
024700100719     C*------------------------------------------------------------------------*
024800100719     C     EXECCM        BEGSR
024900100719     C*
025000100719     C                   movel     'C'           wChkCli           1
025100100719     C*
025200100719     C                   exsr      SQ_CCMDSP
025300100719     C*
025400100719     C                   ENDSR
025500100719     C*------------------------------------------------------------------------*
025600100719
025700100719
025800100719
025900100719     C*------------------------------------------------------------------------*
026000100719     C* EXEKSC   - Elaborazione x codice cliente fatturazione
026100100719     C*------------------------------------------------------------------------*
026200100719     C     EXEKSC        BEGSR
026300100719     C*
026400100719     C                   movel     'K'           wChkCli           1
026500100719     C*
026600100719     C                   exsr      SQ_KSCDSP
026700100719     C*
026800100719     C                   ENDSR
026900100719     C*------------------------------------------------------------------------*
027000100719
027100100719
027200100719
027300100719     C*------------------------------------------------------------------------*
027400100719     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
027500100719     C*------------------------------------------------------------------------*
027600100719     C     SQ_CCMDSP     BEGSR
027700100719     C*
027800100719     C                   z-add     KUNI(I)       wParCCM           7 0
027900100719     C*
028000100719     C/EXEC SQL
028100100719     C+ declare C_CCMDSP cursor for
028200100719     C+ select * from titas00f
028300100719     C+ where tasccm = :wParCCM and
028400100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028500100719     C+ union
028600100719     C+ select * from titas10f
028700100719     C+ where tasccm = :wParCCM and
028800100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028900100719     C+ union
029000100719     C+ select * from titasP0f
029100100719     C+ where tasccm = :wParCCM and
029200100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029300100719     C+ for read only
029400100719     C/END-EXEC
029500100719     C
029600100719     C/EXEC SQL
029700100719     C+ open C_CCMDSP
029800100719     C/END-EXEC
029900100719     C
030000100719     C/EXEC SQL
030100100719     C+ Fetch C_CCMDSP into :TITAS00F
030200100719     C/END-EXEC
030300100719     C*
030400100719     C                   dow       sqlcod = *zeros
030500100719     C                   exsr      verifica
030600100719     C                   if        CHKREC = 'S'
030700100719     C                   exsr      scriviVAB
030800100719     C                   endif
030900100719     C
031000100719     C/EXEC SQL
031100100719     C+ Fetch C_CCMDSP into :TITAS00F
031200100719     C/END-EXEC
031300100719     C*
031400100719     C                   enddo
031500100719     C*
031600100719     C/EXEC SQL
031700100719     C+ close C_CCMDSP
031800100719     C/END-EXEC
031900100719     C*
032000100719     C*
032100100719     C                   ENDSR
032200100719     C*------------------------------------------------------------------------*
032300100719
032400100719
032500100719
032600100719     C*------------------------------------------------------------------------*
032700100719     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
032800100719     C*------------------------------------------------------------------------*
032900100719     C     SQ_KSCDSP     BEGSR
033000100719     C*
033100100719     C                   z-add     KUNI(I)       wParKSC           7 0
033200100719     C*
033300100719     C/EXEC SQL
033400100719     C+ declare C_KSCDSP cursor for
033500100719     C+ select * from titas00f
033600100719     C+ where tasksc = :wParKSC and
033700100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
033800100719     C+ union
033900100719     C+ select * from titas10f
034000100719     C+ where tasksc = :wParKSC and
034100100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
034200100719     C+ union
034300100719     C+ select * from titasP0f
034400100719     C+ where tasksc = :wParKSC and
034500100719     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
034600100719     C+ for read only
034700100719     C/END-EXEC
034800100719     C
034900100719     C/EXEC SQL
035000100719     C+ open C_KSCDSP
035100100719     C/END-EXEC
035200100719     C
035300100719     C/EXEC SQL
035400100719     C+ Fetch C_KSCDSP into :TITAS00F
035500100719     C/END-EXEC
035600100719     C*
035700100719     C                   dow       sqlcod = *zeros
035800100719     C                   exsr      verifica
035900100719     C                   if        CHKREC = 'S'
036000100719     C                   exsr      scriviVAB
036100100719     C                   endif
036200100719     C
036300100719     C/EXEC SQL
036400100719     C+ Fetch C_KSCDSP into :TITAS00F
036500100719     C/END-EXEC
036600100719     C*
036700100719     C                   enddo
036800100719     C*
036900100719     C/EXEC SQL
037000100719     C+ close C_KSCDSP
037100100719     C/END-EXEC
037200100719     C*
037300100719     C*
037400100719     C                   ENDSR
037500100719     C*------------------------------------------------------------------------*
037600100719
037700001218
037800001218
037900010131     C*------------------------------------------------------------------------*
038000161205     C* PROCIMI - Routine principale per richiesta lancio IMI
038100010131     C*------------------------------------------------------------------------*
038200081006     C     PROCIMI       BEGSR
038300010131     C*
038400010131     C* Mi posiziono a inizio file bolle (TITAS) per data iniziale
038500010131     C                   z-add     DATADA        DATAWRK
038600010131     C                   z-add     AAAA          depAAS
038700010131     C                   z-add     MMGG          depMGS
038800010131     C*
038900010131     C     K38C          setll     titas38c
039000010131     C                   read      titas38c
039100010131     C*
039200010131     C                   dow       not %eof(titas38c)
039300010131     C* Verifico di non essere fuori range date
039400010131     C                   z-add     tasaas        AAAA
039500010131     C                   z-add     tasmgs        MMGG
039600010131     C                   if        DATAWRK > DATAA
039700010131     C                   leave
039800010131     C                   else
039900010131     C                   exsr      verifica
040000010131     C                   if        CHKREC = 'S'
040100010131     C                   exsr      scriviVAB
040200010131     C                   endif
040300010131     C                   endif
040400010131     C                   read      titas38c
040500010131     C                   enddo
040600010131     C*
040700010131     C                   ENDSR
040800010131     C*------------------------------------------------------------------------*
040900010131
041000010131
041100001218
041200001218     C*------------------------------------------------------------------------*
041300001218     C* VERIFICA - Routine di verifica validità record corrente
041400001218     C*------------------------------------------------------------------------*
041500001218     C     VERIFICA      BEGSR
041600001218     C*
041700001218     C                   movel     'S'           CHKREC            1
041800001218     C*
041900001218     C* Verifico le date
042000001218     C                   if        CHKREC = 'S'
042100001218     C                   movel     tasaas        wrkdata           8 0
042200001218     C                   move      tasmgs        wrkdata
042300001218     C                   if        wrkdata < DATADA or
042400001218     C                             wrkdata > DATAA
042500001218     C                   movel     'N'           CHKREC
042600001218     C                   endif
042700001218     C                   endif
042800001218     C*
042900010301     C* Verifico il codice cliente per no richiesta IMI (RIDONDANTE)
043000001218     C                   if        CHKREC = 'S'
043100010131     C                   if        FLGIMI <> 'S'
043200010301     C                   if        tasksc <> KUNI(I)
043300010301     C                   movel     'N'           CHKREC
043400001218     C                   endif
043500010131     C                   endif
043600001218     C                   endif
043700010131     C*
043800010131     C* Verifico esistenza IMI qualora richiesto nel lancio
043900010131     C                   if        CHKREC = 'S'
044000010131     C                   if        FLGIMI = 'S'
044100010131     C                   if        tasftc = 'F' or tastc2 = 'F'
044200010131     C                   else
044300010131     C                   movel     'N'           CHKREC
044400010131     C                   endif
044500010131     C                   endif
044600010131     C                   endif
044700100719     C*
044800100719     C* Verifico il codice cliente mittente
044900100719     C                   if        CHKREC = 'S'
045000100719     C                   if        wChkCli  = 'C'      AND
045100100719     C                             tasccm  <> KUNI(I)
045200100719     C                   movel     'N'           CHKREC
045300100719     C                   endif
045400100719     C                   endif
045500100719     C*
045600100719     C* Verifico il codice cliente fatturazione (in caso d 'K')
045700100719     C                   if        CHKREC = 'S'
045800100719     C                   if        wChkCli  = 'K'      AND
045900100719     C                             tasksc  <> KUNI(I)
046000100719     C                   movel     'N'           CHKREC
046100100719     C                   endif
046200100719     C                   endif
046300100719     C*
046400100719     C* Verifico il codice cliente fatturazione (in caso d 'E')
046500100719     C                   if        CHKREC = 'S'
046600100719     C                   if        TIPCLI  = 'E'       AND
046700100719     C                             wChkCli = 'K'       AND
046800100719     C                             tasksc  = tasccm
046900100719     C                   movel     'N'           CHKREC
047000100719     C                   endif
047100100719     C                   endif
047200140702     C*
047300090217     C* Verifico il tipo bolla che non deve essere di reupero
047400090217     C                   if        CHKREC = 'S'
047500090217     C                   if        %lookup(tastbl:tbo)>0
047600090217     C                   movel     'N'           CHKREC
047700090217     C                   endif
047800090217     C                   endif
047900001218     C*
048000001218     C                   ENDSR
048100001218     C*------------------------------------------------------------------------*
048200001218
048300001218
048400001218
048500001218     C*------------------------------------------------------------------------*
048600001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
048700001218     C*------------------------------------------------------------------------*
048800001221     C     SCRIVIVAB     BEGSR
048900001218     C*
049000001227     C                   clear                   fnvab000
049100161205     C                   if        FLGIMI = 'S'
049200010131     C                   seton                                        55
049300010131     C                   else
049400010131     C                   setoff                                       55
049500010131     C                   endif
049600151019     C*
049700151019     C* Eseguo eventuali "forzature"
049800151019     C                   EXSR      RTVAR5FAT
049900160208     C*
050000160208     C* Reprrisco NATURA MERCE
050100160208     C                   EXSR      RTVNAS
050200151019     C*
050300001221     C                   movel     tascbo        vabcbo
050400001221     C                   movel     tasrsd        vabrsd
050500001221     C                   movel     tasind        vabind
050600001221     C                   movel     tascad        vabcad
050700001221     C                   movel     taslod        vablod
050800001221     C                   movel     tasprd        vabprd
050900001221     C                   movel     tasnzd        vabnzd
051000001221     C                   movel     tasgc1        vabgc1
051100001221     C                   movel     tasgc2        vabgc2
051200001221     C                   movel     tastsp        vabtsp
051300160208     C                   movel     §TA4ANAS      vabnas
051400001221     C                   movel     tasctm        vabctm
051500001221     C                   movel     tasffd        vabffd
051600001221     C                   movel     tastcr        vabtcr
051700001221     C                   movel     tascts        vabcts
051800001221     C                   movel     tasftm        vabftm
051900001221     C                   movel     tasgma        vabgma
052000001221     C                   movel     tasgga        vabgga
052100001221     C                   movel     tasgva        vabgva
052200010111     C                   move      ' '           vabgva
052300100927     C                   movel     tasftc        vabtc1
052400100927     C                   movel     tastc2        vabtc2
052500001221     C                   if        tasccm > 0
052600001221     C                   z-add     tasccm        vabccm
052700001221     C                   else
052800001221     C                   z-add     tasksc        vabccm
052900001221     C                   end
053000001221     C                   z-add     taslnp        vablnp
053100001221     C                   z-add     tasaas        vabaas
053200001221     C                   z-add     tasmgs        vabmgs
053300001221     C                   z-add     tasnrs        vabnrs
053400001221     C                   z-add     tasnsp        vabnsp
053500001221     C                   z-add     taslna        vablna
053600001221     C                   z-add     tasctr        vabctr
053700001221     C                   z-add     tasncl        vabncl
053800001221     C                   z-add     taspkf        vabpkb
053900001221     C                   z-add     tasvlf        vabvlb
054000001221     C                   z-add     tasqft        vabqft
054100001221     C                   z-add     tasrmn        vabrmn
054200001221     C                   z-add     tasncd        vabncd
054300001221     C                   z-add     tasnca        vabnca
054400001221     C                   z-add     tasznc        vabznc
054500010108     C                   z-add     tasdbr        vabdcr
054600001221     C* Effettuo considerazioni particolare per bolla "POSTE"
054700001227     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
054800001227     C                   exsr      REPPTIAS
054900001227     C                   else
055000001227     C                   movel     tasIAS        vabIAS
055100001227     C                   movel     tasVAS        vabVAS
055200001227     C                   movel     tasXCO        vabXCO
055300001227     C                   endif
055400001227     C* Considerazioni per bolle "autogenerate"
055500001227     C                   if        tasSOP = '&' and
055600001227     C                             vabSCL = *blanks
055700001227     C                   movel     'A'           vabSCL
055800001227     C                   endif
055900001227     C*
056000001227     C                   exsr      REPCSB
056100010131     C                   exsr      REPRMA
056200081006     C                   exsr      REPANT
056300081006     C                   exsr      REPDEST
056400010605     C*
056500010605     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
056600010605     C                   exsr      REPIMP
056700001218     C*
056800010131     C                   if        *in55 = *off
056900050825     C   25              WRITE     FNVAB000
057000050825     C   26              exsr      WRITIVGD
057100010131     C                   endif
057200001218     C*
057300001218     C                   ENDSR
057400001221     C*------------------------------------------------------------------------*
057500050825
057600050825
057700050825
057800050825     C*------------------------------------------------------------------------*
057900050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
058000050825     C*------------------------------------------------------------------------*
058100050825     C     WRITIVGD      BEGSR
058200050825     C*
058300161206     C                   clear                   tivgd000
058400080407     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
058500100312     C                   eval      vgdTIP = TIPFILE
058600170227     C                   eval      vgdKSU = '0'+%editc(CODCLIVAS:'X')
058700050825     C                   eval      vgdTSC = 'WW'
058800161205     C                   eval      vgdDAT = datcor
058900170227     C                   eval      vgdPRG = o§7VASPRG
059000170227     C                   eval      vgdPGM = procname
059100170227     C                   eval      vgdSTO = '?'
059200161206     C                   write     tivgd000
059300050825     C*
059400050825     C                   ENDSR
059500050825     C*------------------------------------------------------------------------*
059600001221
059700001221
059800001218
059900001218     C*------------------------------------------------------------------------*
060000001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
060100010131     C*          - e dei pacchi ingombranti non dichiarati
060200001218     C*------------------------------------------------------------------------*
060300001218     C     REPPTIAS      BEGSR
060400001218     C*
060500060215     D* Prendo importo da assicurare da TITA4 t.r.=J
060600001218     C                   movel     'J'           KTRC
060700001218     C     KTA4          chain     tita430c
060800001218     C                   if        %found(tita430c)
060900001218     C                   movel     ta4NOT        DSBL4J
061000001221     C                   z-add     §B4IAS        vabIAS
061100001221     C                   movel     §B4VAS        vabVAS
061200001221     C                   movel     §B4XCO        vabXCO
061300001227     C                   movel     §B4BAU        vabSCL
061400010131     C                   if        §B4BAI = 'S'
061500010131     C                   movel     §B4BAI        vabFTM
061600010131     C                   setoff                                       55
061700010131     C                   endif
061800001218     C                   endif
061900001218     C*
062000001218     C                   ENDSR
062100001218     C*------------------------------------------------------------------------*
062200001218
062300001218
062400001218
062500001218     C*------------------------------------------------------------------------*
062600001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
062700001218     C*------------------------------------------------------------------------*
062800001218     C     REPRMA        BEGSR
062900001218     C*
063000060215     D* Prendo importo da assicurare da TITA4 t.r.=A
063100001218     C                   movel     'A'           KTRC
063200001218     C     KTA4          chain     tita430c
063300001218     C                   if        %found(tita430c)
063400001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
063500001218     C                   endif
063600001218     C*
063700001218     C                   ENDSR
063800001218     C*------------------------------------------------------------------------*
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
067200001218     C*------------------------------------------------------------------------*
067300001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
067400001218     C*------------------------------------------------------------------------*
067500001218     C     REPCSB        BEGSR
067600001218     C*
067700001219     C     KCSB          chain     tncsb03l
067800001219     C                   if        %found(tncsb03l)
067900001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
068000001218     C                   else
068100001221     C                   movel     csbTPA        vabTIC
068200001221     C                   move      csbTPI        vabTIC
068300001221     C                   z-add     csbCAS        vabCAS
068400001221     C                   movel     csbVCA        vabVCA
068500001221     C                   movel     csbGCA        vabGCA
068600001218     C                   endif
068700001218     C                   endif
068800001218     C*
068900001218     C                   ENDSR
069000001218     C*------------------------------------------------------------------------*
069100001218
069200010605
069300010605
069400010605     C*------------------------------------------------------------------------*
069500010605     C* REPIMP - Routine di considerazioni/conversioni importi/divise valore da ass. e contrass.
069600010605     C*------------------------------------------------------------------------*
069700010605     C     REPIMP        BEGSR
069800010605     C*
069900010605     C* Come prima cosa verifico se trattasi di bolla POSTE
070000010605     C                   if        vabtsp = 'P'
070100010605     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
070200010605     C                   if        vabvas = 'EUR'
070300010605     C                   eval      vabvmd = vabias
070400010605     C                   eval(h)   wrklire = vabvmd * 1936,27
070500010605     C                   eval      vabias = wrklire
070600010605     C                   eval      vabvas = *blanks
070700010605     C                   endif
070800010605     C*
070900010605     C                   if        vabvca = 'EUR'
071000010605     C                   eval      vabqft = vabcas
071100010605     C                   eval(h)   wrklire = vabqft * 1936,27
071200010605     C                   eval      vabcas = wrklire
071300010605     C                   eval      vabvca = *blanks
071400010605     C                   endif
071500010605     C*
071600010605     C                   eval      vabxco = *blanks
071700010605     C                   endif
071800010605     C*
071900010605     C                   ENDSR
072000030704
072100030704
072200030704
072300030704     C*------------------------------------------------------------------------*
072400030704     C* REPANT - Routine di reperimento dati relativi al numero di bancali
072500030704     C*------------------------------------------------------------------------*
072600030704     C     REPANT        BEGSR
072700030704     C*
072800050825     C                   clear                   ds_vabANT
072900030704     C                   if        %subst(tasGVA:1:1) = 'B'
073000151019     C                   eval      kar5TRD = 'BAN'
073100040531     C     KFIAR5        chain     fiar531c
073200040531     C                   if        %found(fiar531c)
073300030704     C                   movel     ar5UNI        DAR5BAN
073400030704     C                   movel     §ar5TBA       vabTBA
073500030704     C                   movel     §ar5NBA       vabNBA
073600030704     C                   movel     §ar5TB2       vabTB2
073700030704     C                   movel     §ar5NB2       vabNB2
073800030704     C                   endif
073900030704     C                   endif
074000050825     C                   eval      vabANT = ds_vabANT
074100030704     C*
074200030704     C                   ENDSR
074300030704     C*------------------------------------------------------------------------*
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
079500151019
079600151019
079700151019
079800151019     C     RTVAR5FAT     BEGSR
079900151019     C*
080000151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
080100151019     C                   SETOFF                                       7071
080200151019     C                   EVAL      kar5TRD = 'FAT'
080300151019     C                   CLEAR                   DAR5FAT
080400151019     C     KFIAR5        CHAIN     FIAR531C
080500151019     C                   IF        %found(FIAR531C)
080600151019     C                   MOVEL     AR5UNI        DAR5FAT
080700151019     C                   IF        §AR5PKTAS > *zeros
080800151019     C                   SETON                                        70
080900151019     C                   EVAL      tasPKF = §AR5PKTAS
081000151019     C                   ENDIF
081100151019     C                   IF        §AR5VLTAS > *zeros
081200151019     C                   SETON                                        71
081300160505     C                   EVAL      tasVLF = §AR5VLTAS
081400151019     C                   ENDIF
081500151019     C                   ENDIF
081600151019     C*
081700151019     C                   ENDSR
081800160208
081900160208
082000160208
082100160208     C     RTVNAS        BEGSR
082200160208     C*
082300160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
082400160208     C                   CLEAR                   DTA4A
082500160208     C*
082600160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
082700160208     C                   CALL      'UBTA400R'
082800160208     C                   PARM      *blanks       UBTA4IOPZ
082900160208     C                   PARM      *blanks       UBTA4ITLA
083000160208     C                   PARM      tasAAS        UBTA4IAAS
083100160208     C                   PARM      tasLNP        UBTA4ILNP
083200160208     C                   PARM      tasNRS        UBTA4INRS
083300160208     C                   PARM      tasNSP        UBTA4INSP
083400160208     C                   PARM      'A'           UBTA4ITRC
083500160208     C                   PARM                    UBTA4OERR
083600160208     C                   PARM                    UBTA4ODS
083700160208     C                   PARM                    UBTA4OLEN
083800160208     C                   PARM                    UBTA4ODATI
083900160208     C*
084000160208     C                   IF        UBTA4OERR = *zeros
084100160208     C                   SELECT
084200160208     C* Gestione output tipo record 'A'
084300160208     C                   WHEN      UBTA4ODS = 'DTA4A'
084400160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
084500160208     C                   ENDSL
084600160208     C*
084700160208     C                   ENDIF
084800160208     C*
084900160208     C                   ENDSR
085000161205
085100161205
085200161205
085300161205     C*------------------------------------------------------------------------*
085400161205     C* ENDELA - Routine di lanci di chiusura finali
085500161205     C*------------------------------------------------------------------------*
085600161205     C     ENDELA        BEGSR
085700161205     C*
085800161205     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
085900161205     C                   CALL      'UBTA400R'
086000161205     C                   PARM                    UBTA4IOPZ
086100161205     C                   PARM      'C'           UBTA4ITLA
086200161205     C                   PARM                    UBTA4IAAS
086300161205     C                   PARM                    UBTA4ILNP
086400161205     C                   PARM                    UBTA4INRS
086500161205     C                   PARM                    UBTA4INSP
086600161205     C                   PARM                    UBTA4ITRC
086700161205     C                   PARM                    UBTA4OERR
086800161205     C                   PARM                    UBTA4ODS
086900161205     C                   PARM                    UBTA4OLEN
087000161205     C                   PARM                    UBTA4ODATI
087100161205     C*
087200161205     C                   ENDSR
087300161205     C*------------------------------------------------------------------------*
087400161205
087500161205
087600161205
087700161205     C*------------------------------------------------------------------------*
087800161205     C* EXEERR - Routine di esecuzione azioni su Errore
087900161205     C*------------------------------------------------------------------------*
088000161205     C     EXEERR        BEGSR
088100161205     C*
088200161205     C                   dump(A)
088300170322     C                   rolbk(e)
088400161205     C                   seton                                        lr
088500161205     C                   return
088600161205     C*
088700161205     C                   ENDSR
088800161205     C*------------------------------------------------------------------------*
088900001218
089000001218
089100001218
089200001218     C*------------------------------------------------------------------------*
089300001218     C* *INZSR - ROUTINE INIZIALE
089400001218     C*------------------------------------------------------------------------*
089500001218     C     *INZSR        BEGSR
089600001218     C*
089700001218     C     *ENTRY        PLIST
089800140702     C                   PARM                    PARAM            38
089900001218     C*
090000001218     C                   MOVEL     PARAM         DSINPUT
090100001218     C                   Z-ADD     1             CODUT
090200001218     C*
090300001218     C* Definizioni chiavi
090400010131     C     K38C          KLIST
090500010131     C                   KFLD                    depAAS            4 0
090600010131     C                   KFLD                    depMGS            4 0
090700010131     C*
090800001218     C     KTA4          KLIST
090900001218     C                   KFLD                    tasAAS
091000001218     C                   KFLD                    tasLNP
091100001218     C                   KFLD                    tasNRS
091200001218     C                   KFLD                    tasNSP
091300001218     C                   KFLD                    KTRC              1
091400001218     C*
091500001218     C     KTAA          KLIST
091600001218     C                   KFLD                    tasAAS
091700001218     C                   KFLD                    tasLNP
091800001218     C                   KFLD                    tasNRS
091900001218     C                   KFLD                    tasNSP
092000001218     C                   KFLD                    KTRC
092100001218     C*
092200001218     C     KCSB          KLIST
092300001218     C                   KFLD                    tasAAS
092400001218     C                   KFLD                    tasLNP
092500001218     C                   KFLD                    tasNRS
092600001218     C                   KFLD                    tasNSP
092700001218     C*
092800001218     C     KTAB          KLIST
092900001218     C                   KFLD                    CODUT             1 0
093000001218     C                   KFLD                    COD               2
093100030704     C*
093200030704     C     KFIAR5        KLIST
093300030704     C                   KFLD                    tasAAS
093400030704     C                   KFLD                    tasLNP
093500030704     C                   KFLD                    tasNRS
093600030704     C                   KFLD                    tasNSP
093700030704     C                   KFLD                    kar5TRD
093800001218     C*
093900001218     C* Determino la data corrente
094000100312     C                   Z-ADD     *zeros        DATCOR            8 0
094100100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
094200001218     C*
094300001218     C                   ENDSR
