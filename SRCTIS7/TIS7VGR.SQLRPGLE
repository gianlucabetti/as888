000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300001221     H*--------------------------------------------------------------*
000400090130     H*         - RITORNO DATI AL CLIENTE (giacenze)
000500000000     H*--------------------------------------------------------------*
000600001218     FTABEL00F  IF   E           K DISK
000700090130     FTITA430C  IF   E           K DISK
000800090130     FTigcp51L  IF   E           K DISK
000900090130     FTignp01L  IF   E           K DISK
001000161205     FFNVAG00T  UF A E             DISK    USROPN COMMIT
001100161206     FTIVGD00F  O    E             DISK    USROPN COMMIT
001200001218     D*--------------------
001300001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001400001218     D*--------------------
001500090407     D KUNI            S              7  0 DIM(1000) DESCEND
001600001218     D*--------------------
001700001218     D* DS ESTERNE
001800001218     D*--------------------
001900161205     D psds           sds
002000161205     D  procname         *PROC
002100100705     D TITAS00F      E DS
002200900517     D KPJBA         E DS
002300090130     D DS3K          E DS
002400090130     D DS2F          E DS
002500090130     D DS2A          E DS
002600090130     D FNVAGDS       E DS                  EXTNAME(FNVAG00T)
002700161205     D DGCPFLR       E DS
002800170227     D TIS7VASDS     E DS
002900001218     D*--------------------
003000001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003100001218     D*--------------------
003200050825     D DSINPUT         DS
003300050825     D  DATADA                        8  0
003400050825     D  DATAA                         8  0
003500050825     D  CODCLI                        7  0
003600090130     D  FLGgia                        1
003700050825     D  FLGUNI                        1
003800050825     D  FLGOPZ                        1
003900080115     D  CODCLIVAS                     7  0
004000100312     D  TIPFILE                       2
004100080213     D  FLGEXE                        1
004200080213     D  TIPCLI                        1
004300151110     D  TIPDATA                       1
004400920812     C*---------------------------------------------------------------*
004500001218     C* MAIN
004600001218     C*---------------------------------------------------------------*
004700100705     C*
004800100705     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004900100705     C
005000100705     C/EXEC SQL
005100100705     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005200100705     C/END-EXEC
005300100705     C
005400161206     C*
005500161206     C* Avvio il monitoring del processo
005600161206     C                   monitor
005700161206     C*
005800001218     C                   exsr      cartbl
005900001218     C                   exsr      chkpar
006000161205     C                   exsr      procedi
006100161206     C*
006200161206     C* Sabcisco il commit
006300170309     C                   commit(e)
006400161206     C*
006500161206     C* Su errore
006600161206     C                   on-error
006700161206     C                   exsr      exeerr
006800161206     C*
006900161206     C* Fine monitoring
007000161206     C                   endmon
007100001218     C*
007200001218     C                   seton                                        LR
007300001218     C*------------------------------------------------------------------------*
007400001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
007500001218     C*------------------------------------------------------------------------*
007600001218     C     CHKPAR        BEGSR
007700151110     C*
007800151110     C* Inizializzo date giacenza (di comodo)
007900151110     C                   z-add     *zeros        DATADA_GIAC       8 0
008000151110     C                   z-add     *zeros        DATA_A_GIAC       8 0
008100151110     C*
008200151110     C* Verifico il tipo di data richiesto
008300151110     C                   if        TIPDATA = 'X'
008400151110     C                   eval      DATADA_GIAC = DATADA
008500151110     C                   eval      DATA_A_GIAC = DATAA
008600151110     C                   eval      DATADA = *zeros
008700151110     C                   eval      DATAA  = *zeros
008800151110     C                   endif
008900001218     C*
009000001218     C* Verifico le date
009100001218     C                   if        DATAA  = *zeros
009200001218     C                   movel     *all'9'       DATAA
009300001218     C                   endif
009400050825     C*
009500050825     C* Verifico la modalità d scrittura output richiesta:
009600090130     C*   1 = crea membro "RIC" in file FNVAG00T
009700050825     C*   2 = scrivi record in file TIVGD00F
009800050825     C                   setoff                                       2526
009900050825     C                   if        FLGOPZ = '1'
010000050825     C                   seton                                        25
010100050825     C                   endif
010200050825     C                   if        FLGOPZ = '2'
010300050825     C                   seton                                        26
010400050825     C                   endif
010500001218     C*
010600001218     C                   ENDSR
010700001218     C*------------------------------------------------------------------------*
010800001218     C* PROCEDI - Routine principale
010900001218     C*------------------------------------------------------------------------*
011000001218     C     PROCEDI       BEGSR
011100161205     C*
011200161205     C* Inizializzo variabili d wrk
011300161205     C                   movel     'N'           wProcedi          1
011400161205     C*
011500161205     C* Apro il file di output
011600161205     C                   if        *in25
011700161205     C                   open      fnvag00t
011800161205     C                   movel     'S'           wProcedi
011900161205     C                   endif
012000161205     C*
012100161205     C* Inizializzo la transazione
012200161205     C                   if        *in26
012300161206     C                   open      tivgd00f
012400170227     C*
012500170227     C* Stacco progressivo univoco download
012600170227     C                   CLEAR                   TIS7VASDS
012700170227     C                   EVAL      i§7VASOPZ = 'PRG'
012800170227     C                   CALL(e)   'TIS7VASR1'
012900170227     C                   PARM                    TIS7VASDS
013000170227     C*
013100170227     C* Se OK => proseguo
013200170227     C                   if        not %error AND
013300170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
013400170227     C                   movel     'S'           wProcedi
013500170227     C                   endif
013600161206     C                   endif
013700161205     C*
013800161205     C* Se ok a procedere => elaboro
013900161205     C                   if        wProcedi = 'S'
014000001218     C*
014100001218     C* Ciclo sulla schiera dei clienti figli dell'unificante preso in input
014200001218     C                   z-add     1             I
014300090407     C                   sorta     KUNI
014400001218     C                   dow       KUNI(I) > *zeros
014500100705     C*
014600100705     C* Se richiesta elaborazione x codice cliente mittente...
014700100705     C                   select
014800100705     C                   when      TIPCLI = *blanks
014900100705     C                   exsr      EXECCM
015000100705     C*
015100100705     C* Se richiesta elaborazione x codice cliente fatturazione...
015200100705     C                   when      TIPCLI = 'K'
015300100705     C                   exsr      EXEKSC
015400100705     C*
015500100705     C* Se richiesta elaborazione x entrambi: codice cliente mittente + fatturazione...
015600100705     C                   when      TIPCLI = 'E'
015700100705     C                   exsr      EXECCM
015800100705     C                   exsr      EXEKSC
015900100705     C*
016000100705     C                   endsl
016100001218     C*
016200001218     C                   add       1             I
016300001218     C                   enddo
016400161205     C*
016500161205     C                   endif
016600161205     C*
016700161205     C* Chiudo il file di output
016800161205     C   25              close     fnvag00t
016900161206     C   26              close     tivgd00f
017000170227     C*
017100170227     C* Finalizzo la transazione
017200170227     C                   if        *in26
017300170227     C                   EVAL      i§7VASOPZ  = 'RLS'
017400170227     C                   EVAL      i§7VASTIP  = TIPFILE
017500170227     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLIVAS:'X')
017600170227     C                   EVAL      i§7VASTSC  = 'WW'
017700170227     C                   EVAL      i§7VASSTO  = '?'
017800170227     C                   EVAL      i§7VASSTTO = 'L'
017900170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
018000170227     C                   CALL(e)   'TIS7VASR1'
018100170227     C                   PARM                    TIS7VASDS
018200170227     C*
018300170227     C* Verifico esito chiamata
018400170227     C                   if        %error OR o§7VASOK = *off
018500170227     C                   exsr      EXEERR
018600170227     C                   endif
018700170227     C                   endif
018800001218     C*
018900001218     C                   ENDSR
019000100705     C*------------------------------------------------------------------------*
019100100705
019200100705
019300100705
019400100705     C*------------------------------------------------------------------------*
019500100705     C* EXECCM   - Elaborazione x codice cliente mittente
019600100705     C*------------------------------------------------------------------------*
019700100705     C     EXECCM        BEGSR
019800100705     C*
019900100705     C                   movel     'C'           wChkCli           1
020000100705     C*
020100100705     C                   exsr      SQ_CCMDSP
020200100705     C*
020300100705     C                   ENDSR
020400100705     C*------------------------------------------------------------------------*
020500100705
020600100705
020700100705
020800100705     C*------------------------------------------------------------------------*
020900100705     C* EXEKSC   - Elaborazione x codice cliente fatturazione
021000100705     C*------------------------------------------------------------------------*
021100100705     C     EXEKSC        BEGSR
021200100705     C*
021300100705     C                   movel     'K'           wChkCli           1
021400100705     C*
021500100705     C                   exsr      SQ_KSCDSP
021600100705     C*
021700100705     C                   ENDSR
021800100705     C*------------------------------------------------------------------------*
021900100705
022000100705
022100100705
022200100705     C*------------------------------------------------------------------------*
022300100705     C* SQ_CCMDSP - Elaborazione x codice cliente mittente + data spedizione
022400100705     C*------------------------------------------------------------------------*
022500100705     C     SQ_CCMDSP     BEGSR
022600100705     C*
022700100705     C                   z-add     KUNI(I)       wParCCM           7 0
022800100705     C*
022900100705     C/EXEC SQL
023000100705     C+ declare C_CCMDSP cursor for
023100100705     C+ select * from titas00f
023200100705     C+ where tasccm = :wParCCM and
023300100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
023400100705     C+ union
023500100705     C+ select * from titas10f
023600100705     C+ where tasccm = :wParCCM and
023700100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
023800100705     C+ union
023900100705     C+ select * from titasP0f
024000100705     C+ where tasccm = :wParCCM and
024100100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
024200100705     C+ for read only
024300100705     C/END-EXEC
024400100705     C
024500100705     C/EXEC SQL
024600100705     C+ open C_CCMDSP
024700100705     C/END-EXEC
024800100705     C
024900100705     C/EXEC SQL
025000100705     C+ Fetch C_CCMDSP into :TITAS00F
025100100705     C/END-EXEC
025200100705     C*
025300100705     C                   dow       sqlcod = *zeros
025400100705     C                   exsr      verifica
025500100705     C                   if        CHKREC = 'S'
025600100705     C                   exsr      scriviVAG
025700100705     C                   endif
025800100705     C
025900100705     C/EXEC SQL
026000100705     C+ Fetch C_CCMDSP into :TITAS00F
026100100705     C/END-EXEC
026200100705     C*
026300100705     C                   enddo
026400100705     C*
026500100705     C/EXEC SQL
026600100705     C+ close C_CCMDSP
026700100705     C/END-EXEC
026800100705     C*
026900100705     C*
027000100705     C                   ENDSR
027100100705     C*------------------------------------------------------------------------*
027200100705
027300100705
027400100705
027500100705     C*------------------------------------------------------------------------*
027600100705     C* SQ_KSCDSP - Elaborazione x codice cliente fatturazione + data spedizione
027700100705     C*------------------------------------------------------------------------*
027800100705     C     SQ_KSCDSP     BEGSR
027900100705     C*
028000100705     C                   z-add     KUNI(I)       wParKSC           7 0
028100100705     C*
028200100705     C/EXEC SQL
028300100705     C+ declare C_KSCDSP cursor for
028400100705     C+ select * from titas00f
028500100705     C+ where tasksc = :wParKSC and
028600100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
028700100705     C+ union
028800100705     C+ select * from titas10f
028900100705     C+ where tasksc = :wParKSC and
029000100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029100100705     C+ union
029200100705     C+ select * from titasP0f
029300100705     C+ where tasksc = :wParKSC and
029400100705     C+ tasaas*10000+tasmgs between :DATADA and :DATAA
029500100705     C+ for read only
029600100705     C/END-EXEC
029700100705     C
029800100705     C/EXEC SQL
029900100705     C+ open C_KSCDSP
030000100705     C/END-EXEC
030100100705     C
030200100705     C/EXEC SQL
030300100705     C+ Fetch C_KSCDSP into :TITAS00F
030400100705     C/END-EXEC
030500100705     C*
030600100705     C                   dow       sqlcod = *zeros
030700100705     C                   exsr      verifica
030800100705     C                   if        CHKREC = 'S'
030900100705     C                   exsr      scriviVAG
031000100705     C                   endif
031100100705     C
031200100705     C/EXEC SQL
031300100705     C+ Fetch C_KSCDSP into :TITAS00F
031400100705     C/END-EXEC
031500100705     C*
031600100705     C                   enddo
031700100705     C*
031800100705     C/EXEC SQL
031900100705     C+ close C_KSCDSP
032000100705     C/END-EXEC
032100100705     C*
032200100705     C*
032300100705     C                   ENDSR
032400100705     C*------------------------------------------------------------------------*
032500100705
032600100705
032700100705
032800001218     C*------------------------------------------------------------------------*
032900001218     C* VERIFICA - Routine di verifica validità record corrente
033000001218     C*------------------------------------------------------------------------*
033100001218     C     VERIFICA      BEGSR
033200001218     C*
033300001218     C                   movel     'S'           CHKREC            1
033400001218     C*
033500001218     C* Verifico le date
033600001218     C                   if        CHKREC = 'S'
033700001218     C                   movel     tasaas        wrkdata           8 0
033800001218     C                   move      tasmgs        wrkdata
033900001218     C                   if        wrkdata < DATADA or
034000001218     C                             wrkdata > DATAA
034100001218     C                   movel     'N'           CHKREC
034200001218     C                   endif
034300001218     C                   endif
034400100705     C*
034500100705     C* Verifico il codice cliente mittente
034600100705     C                   if        CHKREC = 'S'
034700100705     C                   if        wChkCli  = 'C'      AND
034800100705     C                             tasccm  <> KUNI(I)
034900100705     C                   movel     'N'           CHKREC
035000100705     C                   endif
035100100705     C                   endif
035200100705     C*
035300100705     C* Verifico il codice cliente fatturazione (in caso d 'K')
035400100705     C                   if        CHKREC = 'S'
035500100705     C                   if        wChkCli  = 'K'      AND
035600100705     C                             tasksc  <> KUNI(I)
035700100705     C                   movel     'N'           CHKREC
035800100705     C                   endif
035900100705     C                   endif
036000100705     C*
036100100705     C* Verifico il codice cliente fatturazione (in caso d 'E')
036200100705     C                   if        CHKREC = 'S'
036300100705     C                   if        TIPCLI  = 'E'       AND
036400100705     C                             wChkCli = 'K'       AND
036500100705     C                             tasksc  = tasccm
036600100705     C                   movel     'N'           CHKREC
036700100705     C                   endif
036800100705     C                   endif
036900100705     C*
037000090130     C* Verifico se spedizione con giacenza
037100090130     C                   if        CHKREC = 'S'
037200101129     C***                if        tasfgc = ' '
037300101129     C***                movel     'N'           CHKREC
037400101129     c***                else
037500090130     c* solo ultimo progressivo
037600090202     c                   clear                   gcpfrg
037700090130     c     kgcp          chain     tigcp51l
037800090130     c                   if        not %found(tigcp51l)
037900090130     C                   movel     'N'           CHKREC
038000090130     c                   else
038100090130     c                   select
038200090130     c* solo giacenze aperte
038300090130     c                   when      flggia = 'A' and gcpfas >= 50
038400090130     C                   movel     'N'           CHKREC
038500090130     c* solo giacenze chiuse
038600090130     c                   when      flggia = 'C' and gcpfas < 50
038700090130     C                   movel     'N'           CHKREC
038800090130     c                   endsl
038900151110     C*
039000151110     C* Se richiesto nel lancio verifico anche la data di giacenza
039100151110     C                   if        TIPDATA = 'X'
039200151110     C                   if        GCPDUR < DATADA_GIAC or
039300151110     C                             GCPDUR > DATA_A_GIAC
039400151110     C                   movel     'N'           CHKREC
039500151110     C                   endif
039600151110     C                   endif
039700151110     C*
039800090130     C                   endif
039900101129     C***                endif
040000090130     C                   endif
040100001218     C*
040200001218     C                   ENDSR
040300001218     C*------------------------------------------------------------------------*
040400090130     C* SCRIVIVAG - Routine di scrittura file giacenze (FNVAG)
040500001218     C*------------------------------------------------------------------------*
040600090130     C     SCRIVIVAG     BEGSR
040700001218     C*
040800090130     C                   clear                   fnvag000
040900090130     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
041000090130     C                   Z-ADD     10            fase
041100090130     C                   Z-ADD     1             NPR
041200090130     C     KNOT          CHAIN     tignp01L
041300090130     c                   if        %found(tignp01l)
041400090130     C                   MOVEL     GNPDMC        ULTDMC
041500090130     c                   else
041600090130     C                   MOVEL     *BLANKS       ULTDMC           50
041700090130     c                   end
041800090130     c* verifico se comunicazione esterno
041900090130     C                   MOVEL     '2F'          tblcod
042000090130     C                   MOVEL(p)  GCPTCM        tblkey
042100090130     c                   clear                   ds2f
042200090130     c                   clear                   tbluni
042300090130     c                   clear                   d08est
042400090130     C     KTAB1         CHAIN     TABEL00f
042500090130     c                   if        %found(tabel00f)
042600090130     C                   MOVEL     TBLUNI        DS2F
042700090130     c                   if        §2Fris =*blank
042800090130     c                   movel     §2FTXT        d08est            1
042900090130     c                   end
043000090130     c                   end
043100090130     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
043200090130     c                   if        d08est = 'E'
043300090130     C                   MOVEL     '2X'          tblcod
043400090130     c                   else
043500090130     C                   MOVEL     '2A'          tblcod
043600090130     c                   end
043700090130     C                   MOVEL(p)  GCPCMC        tblkey
043800090130     c                   clear                   ds2a
043900090130     c                   clear                   tbluni
044000090202     c                   clear                   wmotev
044100090130     C     KTAB1         CHAIN     TABEL00f
044200090130     c                   if        %found(tabel00f)
044300090130     c                   if        d08est = 'E'
044400090130     C                   MOVEL     TBLUNI        WMOTEV           75
044500090130     c                   else
044600090130     C                   MOVEL     TBLUNI        DS2A
044700090130     C     §2ADES        IFEQ      *BLANKS
044800090130     C                   MOVEL     ULTDMC        §2ADES
044900090130     C                   MOVEL     *BLANKS       ULTDMC
045000090130     C                   END
045100090130     c                   end
045200090130     c                   end
045300001218     C*
045400090130     C                   Z-ADD     GCPLNP        VAGLNP
045500090130     C                   Z-ADD     GCPAAS        VAGAAS
045600090130     C                   Z-ADD     GCPNRS        VAGNRS
045700090130     C                   Z-ADD     GCPNSP        VAGNSP
045800090130     C                   Z-ADD     GCPLNA        VAGLNA
045900090130     C                   Z-ADD     GCPAGC        VAGAGC
046000090130     C                   Z-ADD     GCPMGC        VAGMGC
046100090130     C                   Z-ADD     GCPFGC        VAGFGC
046200090130     C                   Z-ADD     GCPNGC        VAGNGC
046300090130     C                   MOVEL     GCPCMC        VAGCMC
046400090204     C                   MOVEL     GCPflr        dgcpflr
046500090204      * ulteriore informazione APPUNTAMENTO
046600090204     c                   if        D§GCPAPP = 'A' and gcpfas < 50
046700090204     C                   Z-ADD     GCPded        VAGded
046800090204     C                   Z-ADD     GCPddm        VAGddm
046900090204     c                   endif
047000090130      *
047100090130     c                   if        gcpded > datcor
047200090130     C                   Z-ADD     GCPDED        VAGDED
047300090130     c                   else
047400090130     C                   move      *zero         VAGDED
047500090130     c                   endif
047600090130      * blocco di campi da scrivere solo se rekord con dati di chiusura
047700090204     c                   if        gcpfas >= 50
047800090130     C                   move      GCPVCS        VAGVCS
047900090130     C                   Z-ADD     GCPCAS        VAGCAS
048000090130     C                   MOVE      GCPVCA        VAGVCA
048100090130     C                   Z-ADD     GCPDDM        VAGDDM
048200090130     C                   Z-ADD     GCPDED        VAGDED
048300090130     C                   Z-ADD     GCPDCG        VAGDCG
048400090130     C                   MOVE      GCPCFG        VAGCFG
048500090130     C                   Z-ADD     GCPDSD        VAGDSD
048600090130     C                   MOVEL     GCPDSA        VAGDSA
048700090130     C                   Z-ADD     GCPGGS        VAGGGS
048800090130     C                   Z-ADD     GCPSGF        VAGSGF
048900090130     C                   Z-ADD     GCPSGA        VAGSGA
049000090130     C                   Z-ADD     GCPTFT        VAGTFT
049100090130     C                   Z-ADD     GCPDBG        VAGDBG
049200090130     C                   Z-ADD     GCPLPW        VAGLPW
049300090130     C                   Z-ADD     GCPASW        VAGASW
049400090130     C                   Z-ADD     GCPNSW        VAGNSW
049500090130     C                   Z-ADD     GCPNBW        VAGNBW
049600090130     C                   Z-ADD     GCPKSC        VAGKSC
049700090130     c                   endif
049800090130     C*
049900090130     C     d08est        IFEQ      'E'
050000090130     C                   MOVEL     WMOTEV        VAGDMC
050100090130     C                   ELSE
050200090130     C                   MOVEL     §2ADES        VAGDMC
050300090130     C                   END
050400090130     C                   MOVEL     ULTDMC        VAGNOT
050500090130     C                   Z-ADD     tasRMN        VAGRMN
050600090130     C                   exsr      REPRMA
050700090130     C                   Z-ADD     GCPSCM        VAGSCM
050800090130     C                   Z-ADD     GCPDMM        VAGDMM
050900090130     C                   MOVEL     tasGGA        VAGGGA
051000090130      * imposto vagdur solo se riapertura vera testando il flag
051100090130     c                   if        gcpriap <> *blank
051200090130     C                   Z-ADD     GCPDUR        VAGDUR
051300090130     c                   endif
051400090130     C                   Z-ADD     0             VAGFRG
051500001218     C*
051600090130     C   25              WRITE     FNVAG000
051700050825     C   26              exsr      WRITIVGD
051800001218     C*
051900001218     C                   ENDSR
052000090130     C*------------------------------------------------------------------------*
052100090130     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
052200090130     C*------------------------------------------------------------------------*
052300090130     C     REPRMA        BEGSR
052400090130     C*
052500090130     D* Prendo importo da assicurare da TITA4 t.r.=A
052600090130     C                   movel     'A'           KTRC
052700090130     C     KTA4          chain     tita430c
052800090130     C                   if        %found(tita430c)
052900090130     C                   eval      vaGRMA = %subst(ta4NOT:1:15)
053000090130     C                   endif
053100090130     C*
053200090130     C                   ENDSR
053300050825     C*------------------------------------------------------------------------*
053400050825     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
053500050825     C*------------------------------------------------------------------------*
053600050825     C     WRITIVGD      BEGSR
053700050825     C*
053800161206     C                   clear                   tivgd000
053900090130     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
054000100312     C                   eval      vgdTIP = TIPFILE
054100170227     C                   eval      vgdKSU = '0'+%editc(CODCLIVAS:'X')
054200050825     C                   eval      vgdTSC = 'WW'
054300050825     C                   eval      vgdDAT = datcor
054400170227     C                   eval      vgdPRG = o§7VASPRG
054500170227     C                   eval      vgdPGM = procname
054600170227     C                   eval      vgdSTO = '?'
054700161206     C                   write     tivgd000
054800050825     C*
054900050825     C                   ENDSR
055000001218     C*------------------------------------------------------------------------*
055100001218     C* CARTBL - Routine di caricamento dati tabellati
055200001218     C*------------------------------------------------------------------------*
055300001218     C     CARTBL        BEGSR
055400001218     C*
055500001221     C                   Z-ADD     0             I                 4 0
055600050825     C                   IF        FLGUNI = 'S'
055700010301     C                   ADD       1             I
055800050825     C                   MOVEL     CODCLI        KUNI(I)
055900010301     C                   ELSE
056000090130     C                   MOVEL     '3K'          COD
056100001218     C     KTAB          CHAIN     TABEL                              31
056200001218     C     *IN31         DOWEQ     '0'
056300090202     C                   IF        TBLFLG = ' '
056400090130     C                   MOVEL     TBLUNI        DS3K
056500090130     C     §3kgks        IFEQ      CODCLI
056600001221     C                   ADD       1             I
056700090202     C                   MOVEL     TBLKEY        wKUNI             7 0
056800090202     C     wKUNI         LOOKUP    KUNI                                   55
056900090202     C                   IF        %equal
057000090202     C                   ELSE
057100090202     C                   Z-ADD     wKUNI         KUNI(I)
057200090202     C                   ENDIF
057300001221     C                   ENDIF
057400001218     C                   ENDIF
057500001218     C     KTAB          READE     TABEL                                  31
057600001218     C                   ENDDO
057700010301     C                   ENDIF
057800001218     C*
057900001218     C                   ENDSR
058000161205
058100161205
058200161205
058300161205     C*------------------------------------------------------------------------*
058400161205     C* EXEERR - Routine di esecuzione azioni su Errore
058500161205     C*------------------------------------------------------------------------*
058600161205     C     EXEERR        BEGSR
058700161205     C*
058800161205     C                   dump(A)
058900170322     C                   rolbk(e)
059000161205     C                   seton                                        lr
059100161205     C                   return
059200161205     C*
059300161205     C                   ENDSR
059400161205     C*------------------------------------------------------------------------*
059500161205
059600161205
059700161205
059800001221     C*------------------------------------------------------------------------*
059900001218     C* *INZSR - ROUTINE INIZIALE
060000001218     C*------------------------------------------------------------------------*
060100001218     C     *INZSR        BEGSR
060200001218     C*
060300001218     C     *ENTRY        PLIST
060400151110     C                   PARM                    PARAM            38
060500001218     C*
060600001218     C                   MOVEL     PARAM         DSINPUT
060700001218     C                   Z-ADD     1             CODUT
060800001218     C*
060900001218     C* Definizioni chiavi
061000010131     C     K38C          KLIST
061100010131     C                   KFLD                    depAAS            4 0
061200010131     C                   KFLD                    depMGS            4 0
061300010131     C*
061400090130     C     Kgcp          KLIST
061500001218     C                   KFLD                    tasAAS
061600001218     C                   KFLD                    tasLNP
061700001218     C                   KFLD                    tasNRS
061800001218     C                   KFLD                    tasNSP
061900090130     C                   KFLD                    gcpfrg
062000090130     C     KNOT          KLIST
062100090130     C                   KFLD                    GCPAGC
062200090130     C                   KFLD                    GCPFGC
062300090130     C                   KFLD                    GCPNGC
062400090130     C                   KFLD                    GCPFRG
062500090130     C                   KFLD                    FASE              3 0
062600090130     C                   KFLD                    NPR               3 0
062700001218     C*
062800001218     C     KTAB          KLIST
062900001218     C                   KFLD                    CODUT             1 0
063000001218     C                   KFLD                    COD               2
063100090130     C     KTAB1         KLIST
063200090130     C                   KFLD                    CODUT             1 0
063300090130     C                   KFLD                    tblcod
063400090130     C                   KFLD                    tblkey
063500090130     C*
063600090130     C     KTA4          KLIST
063700090130     C                   KFLD                    tasAAS
063800090130     C                   KFLD                    tasLNP
063900090130     C                   KFLD                    tasNRS
064000090130     C                   KFLD                    tasNSP
064100090130     C                   KFLD                    KTRC              1
064200001218     C*
064300001218     C* Determino la data corrente
064400100312     C                   Z-ADD     *zeros        DATCOR            8 0
064500100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
064600001218     C*
064700001218     C                   ENDSR
