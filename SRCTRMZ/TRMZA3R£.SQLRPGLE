000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200120301     H**********************************************************************
000300120301      *
000400120301     c*  CONTROLLI INCROCIATI fra tariffe AFF/DEF e TRAZIONISTI
000500120301      *
000600120301      * A) Le tariffe AFF/DEF che hanno Nr.Contratto <> 9999999
000700120301      *   (tramite la Partita IVA) si verifica se:
000800120301      *    trattasi di TRAZIONISTI e NON sono allineati come Tariffe.
000900120301      *   Possiamo avere 2 casi:
001000120301      *     1) il nr.contratto = 0 ->  quindi dovrà essere impostato a 9999999
001100120301      *     2) il nr.contratto > 0 ->  è erroneamente legato a contratto di città
001200120301      *
001300120301      * B) Le tariffe AFF/DEF che hanno invece Nr.Contratto = 9999999 e NON sono
001400120301      *    su anagrafica TRAZIONISTI oppure ci sono e il TRAZIONISTA ha data di
001500120301      *    fine rapporto -> La tariffa deve essere fatta scadere.
001600120301      *
001700020409     H**********************************************************************
001800120313     faitra05l  if   e           k disk
001900120313     ffiapd01l  if   e           k disk
002000120301     fTrmza3P   o    E             PRINTER OFLIND(*in66)
002100040820     H**********************************************************************
002200040820     d KPJBA         E DS
002300040820     D*
002400120301     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500120301     D CNCR80        E DS
002600120301     D*
002700120301     D FIADT00F      e DS
002800120306     D dsAIATS       e DS                  EXTNAME(AIATS00F)
002900120301     D*
003000120301     D trmz70s4ds    E DS                  prefix(S4_)
003100120301
003200120502     D partraz         DS
003300120518     D  parsoc                        3s 0
003400120502     D  parivat                      16
003500120502     D  pardtin                       8s 0
003600120502     D  pardtfi                       8s 0
003700120502     d  esitot                        1
003800120502     d  errmsg                       75
003900120301
004000040820     D WLBDAT          DS
004100040820     D  G02DAT                 1      8  0
004200040820     D  G02INV                 9     16  0
004300040820     D  G02ERR                17     17
004400040820     D  G02TGI                18     22  0
004500040820      *
004600120301     D  savpdr         s                   like(adtpdr)
004700120301      *
004800040820     D WrkSqlCmd       S           1024
004900120301      *
005000040820     D  data_Oggi      s                   like(G02INV) INZ(0)
005100120301     D Trazionista     s              2
005200040820      *
005300120301     D SI              C                   CONST('SI')
005400120301     D NO              C                   CONST('NO')
005500040820     D digits          C                   CONST('0123456789')
005600120313     D datmask         C                   CONST('    /  / 0')
005700040820      *****************************************************************
005800120301      **
005900120301     c* compongo istruzione sql per il TEST (A)
006000120301     c                   exsr      istruzA
006100120301     c                   exsr      Exec__A
006200120301      **
006300120301     c* compongo istruzione sql per il TEST (B)
006400120301     c                   exsr      istruzB
006500120301     c                   exsr      Exec__B
006600120301      ***
006700020326     C                   SETON                                        LR
006800040820     C**************************************************************************
006900040820     C* INIZIALIZZAZIONE
007000040820     C**************************************************************************
007100040820     C     *inzsr        BEGSR
007200040820      **
007300040820     C     *ENTRY        PLIST
007400040820     C                   PARM                    KPJBA
007500041103      *
007600041103     C                   Z-ADD     1             CODUT
007700041103     C                   CALL      'X§PARUT'
007800041103     C                   PARM                    UT§DSE
007900041103     C                   MOVEL     REC80         CNCR80
008000041103     C                   MOVEL     RAGUT         RSUT             20
008100040820      *
008200040820      *  imposta UDATE di elaborazione
008300040820     C                   move      *date         G02DAT
008400040820     C                   MOVE      *ZEROS        G02INV
008500040820     C                   MOVE      *BLANKS       G02ERR
008600040820     C                   CALL      'XSRDA8'
008700040820     C                   PARM                    WLBDAT
008800040820     C                   Z-ADD     G02INV        data_oggi
008900120302     C                   move      *date         day
009000040820     C                   TIME                    W0140            14 0
009100040820     C                   MOVEl     W0140         ore
009200040823      **
009300040823     c                   seton                                        66
009400040825     c                   clear                   prima_volta       1
009500120301     c                   clear                   almeno_uno        1
009600040820     c*
009700040820     C                   ENDSR
009800040820     C**************************************************************************
009900120301     C* Compone l'istruzione sql x il test (A)
010000040820     C**************************************************************************
010100120301     C     istruzA       BEGSR                                                  *
010200040820      **
010300120301     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file FIADT (tariffe affl/defl)
010400040820     C                   EVAL      WrkSqlCmd
010500040820     C                             =
010600120301     C                             'SELECT * from FIADT00F where ADTNRC '
010700120607     c                              + ' < '
010800120607     c                              + ' 9000000 and ADTDST >= '
010900120301     c                              + %trim(%char(data_oggi))
011000120301     c                              + ' ORDER BY ADTPDR '
011100120607      **
011200040820     C                   ENDSR
011300120301     C**************************************************************************
011400120301     C* esegue l'istruzione sql per il TEST (A)
011500120301     C**************************************************************************
011600120301     C     Exec__A       BEGSR                                                  *
011700120301      **
011800120301     c                   move      'A'           RoutineAB         1
011900120301     C/EXEC SQL
012000120301     C+ PREPARE S1 FROM :WrkSqlCmd
012100120301     C/END-EXEC
012200120301
012300120301     C/EXEC SQL
012400120301     C+ DECLARE A1 CURSOR FOR S1
012500120301     C/END-EXEC
012600120301
012700120301     C/EXEC SQL
012800120301     C+ OPEN A1
012900120301     C/END-EXEC
013000120301     C                   DOU       SqlCod <> 0
013100120301     c*  cerca le tariffe con Nr.Contratto <> 9999999 per verificare
013200120301     c*   (tramite la Partita IVA) se trattasi di TRAZIONISTI.
013300120301     c*  Qui si possono trovare 2 casi:
013400120301     c*     1) il nr.contratto = 0 ->  quindi deve essere impostato con 9999999
013500120301     c*     2) il nr.contratto > 0 ->  quindi è legato ad un contratto di città
013600120301     C/EXEC SQL
013700120301     C+ FETCH NEXT FROM A1 INTO :fiadt00f
013800120301     C/END-EXEC
013900120301     c                   exsr      Exec_AB
014000120301      ***
014100120301      **  x FineFile o errore
014200120301     C                   if        SqlCod = 100 or *inH1 = *ON
014300120301     c                   leave
014400120301     c                   end
014500120301      ***
014600120301     C                   ENDDO
014700120301     C/EXEC SQL
014800120301     C+ CLOSE A1
014900120301     C/END-EXEC
015000120301      **
015100120301     C                   ENDSR
015200120301     C**************************************************************************
015300120301     C* Compone l'istruzione sql x il test (B)
015400120301     C**************************************************************************
015500120301     C     istruzB       BEGSR                                                  *
015600120301      **
015700120301     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file FIADT (tariffe affl/defl)
015800120301     C                   EVAL      WrkSqlCmd
015900120301     C                             =
016000120301     C                             'SELECT * from FIADT00F where ADTNRC '
016100120607     c                              + ' >= '
016200120607     c                              + ' 9000000 and ADTDST >= '
016300120301     c                              + %trim(%char(data_oggi))
016400120301     c                              + ' ORDER BY ADTPDR '
016500120607      **
016600120301     C                   ENDSR
016700120301     C**************************************************************************
016800120301     C* esegue l'istruzione sql per il TEST (B)
016900120301     C**************************************************************************
017000120301     C     Exec__B       BEGSR                                                  *
017100120301      **
017200120301     c                   move      'B'           RoutineAB
017300120301     C/EXEC SQL
017400120301     C+ PREPARE S2 FROM :WrkSqlCmd
017500120301     C/END-EXEC
017600120301
017700120301     C/EXEC SQL
017800120301     C+ DECLARE B1 CURSOR FOR S2
017900120301     C/END-EXEC
018000120301
018100120301     C/EXEC SQL
018200120301     C+ OPEN B1
018300120301     C/END-EXEC
018400120301     C                   DOU       SqlCod <> 0
018500120301     c*  cerca le tariffe con Nr.Contratto = 9999999 per verificare
018600120301     c*   (tramite la Partita IVA)
018700120301     c*     1) se NON trattasi di TRAZIONISTA
018800120301     c*     2) se il trazionista ha o aveva già la data di FINE rapporto.
018900120301     C/EXEC SQL
019000120301     C+ FETCH NEXT FROM B1 INTO :fiadt00f
019100120301     C/END-EXEC
019200120301     c                   exsr      Exec_AB
019300120301      ***
019400120301      **  x FineFile o errore
019500120301     C                   if        SqlCod = 100 or *inH1 = *ON
019600120301     c                   leave
019700120301     c                   end
019800120301      ***
019900120301     C                   ENDDO
020000120301     C/EXEC SQL
020100120301     C+ CLOSE B1
020200120301     C/END-EXEC
020300120301      **
020400120301     C                   ENDSR
020500120301     C**************************************************************************
020600120301     C* esegue l'istruzione sql per il TEST (A)
020700120301     C**************************************************************************
020800120301     C     Exec_AB       BEGSR                                                  *
020900120301      **
021000120301     C                   SELECT
021100120301     c*
021200120301     c* a EOF o x rotture di livello
021300120301     C                   WHEN      SqlCod = 100
021400120301      *
021500120301     c                   if        almeno_uno ='S'
021600120503     c                   exsr      FineFile
021700120301     c                   else
021800120301      *
021900120504     c                   if        RoutineAB = 'B'
022000120504     c                   if        *in66
022100120504      *                 * ------------------ *
022200120301     c                   write     testa
022300120301     c                   write     tesdetta
022400120504      *                 * ------------------ *
022500120504     c                   setoff                                       66
022600120504     c                   else
022700120504     c                   seton                                        66
022800120504     c                   end
022900120504      **
023000120301     c                   write     NODATI
023100120301     c                   end
023200120504     c                   end
023300120301      *
023400120503     c                   leavesr
023500120301     **
023600120301     C                   WHEN      SqlCod < 0
023700120301     C                   seton                                        H1
023800120503     c                   leavesr
023900120301     c*
024000120301     C                   OTHER
024100120301      * DETTAGLIO
024200120301     c                   if        *in66
024300120301      *                 * ------------------ *
024400120301     c                   write     testa
024500120301     c                   write     tesdetta
024600120301      *                 * ------------------ *
024700120301     c                   setoff                                       66
024800120301     c                   eval      prima_volta = 'N'
024900120301     c                   end
025000120301      *
025100120301      **  pulisce le descrizioni da Decodificare
025200120301     c                   eval        DESFRN = *blank
025300120301     c                   eval        PIVA   = *blank
025400120301     c                   eval        DESPDR = *blank
025500120301     c                   eval        NOTE   = *blank
025600120313     c                   clear                   datDIS
025700120313     c                   clear                   datUVA
025800120301      *
025900120301      * ricava la Partita IVA
026000120301     c                   exsr      Decod_P_IVA
026100120301      *
026200120301      **  con P.IVA controlla se è un trazionista
026300120301     c                   exsr      se_TRAZIONISTA
026400120301      *
026500120313      **  se non è un trazionista controlla se DIsaccreditato
026600120508     c                   clear                   DATTACC
026700120508     c                   clear                   DATTDIS
026800120508     c                   clear                   DATTNRC
026900120511     c                   clear                   DATTDFC
027000120508      *
027100120313     c                   if        Trazionista = NO
027200120313     c                   exsr      se_DISACCRED
027300120508     c                   else
027400120508      *
027500120508     c                   end
027600120508      * date TRAINI
027700120508      *
027800120508     C                   move      *zeros        G02DAT
027900120508     C                   MOVE      atsDTINC      G02INV
028000120508     C                   MOVE      '3'           G02ERR
028100120508     C                   CALL      'XSRDA8'
028200120508     C                   PARM                    WLBDAT
028300120508     C                   Z-ADD     G02dat        datTACC
028400120508      *
028500120508     C                   move      *zeros        G02DAT
028600120508     C                   MOVE      atsDTFIC      G02INV
028700120508     C                   MOVE      '3'           G02ERR
028800120508     C                   CALL      'XSRDA8'
028900120508     C                   PARM                    WLBDAT
029000120508     C                   Z-ADD     G02dat        datTDIS
029100120508      *
029200120508     C                   move      *zeros        G02DAT
029300120508     C                   MOVE      atsDEC        G02INV
029400120508     C                   MOVE      '3'           G02ERR
029500120508     C                   CALL      'XSRDA8'
029600120508     C                   PARM                    WLBDAT
029700120508     C                   Z-ADD     G02dat        datTNRC
029800120511      *
029900120511     C                   move      *zeros        G02DAT
030000120511     C                   MOVE      atsDFC        G02INV
030100120511     C                   MOVE      '3'           G02ERR
030200120511     C                   CALL      'XSRDA8'
030300120511     C                   PARM                    WLBDAT
030400120511     C                   Z-ADD     G02dat        datTDFC
030500120313      *
030600120313      * Ultima Vlaorizzazione
030700120313     c                   exsr      Ultima_VALOR
030800120313      *
030900120301      *     Controlli specifici
031000120301     c                   if        RoutineAB = 'A'
031100120301     c                   exsr      contr__A
031200120301     c                   else
031300120301     c                   exsr      contr__B
031400120301     c                   end
031500120301      *
031600120301      *  scrive la riga di segnalazione
031700120301     c                   if        NOTE <> *BLANKs
031800120301      *
031900120301      *      Decodifica l'Autista della Tariffa
032000120301     c                   exsr      Anagraf_D
032100120301      *
032200120302     C                   move      *zeros        G02DAT
032300120302     C                   MOVE      adtDDT        G02INV
032400120302     C                   MOVE      '3'           G02ERR
032500120302     C                   CALL      'XSRDA8'
032600120302     C                   PARM                    WLBDAT
032700120302     C                   Z-ADD     G02dat        adtDDT
032800120302      *
032900120302     C                   move      *zeros        G02DAT
033000120302     C                   MOVE      adtDST        G02INV
033100120302     C                   MOVE      '3'           G02ERR
033200120302     C                   CALL      'XSRDA8'
033300120302     C                   PARM                    WLBDAT
033400120302     C                   Z-ADD     G02dat        adtDST
033500120302      *
033600120301     c                   write     dettagli
033700120301     c                   move      'S'           almeno_uno
033800120301     c                   end
033900120301     c*
034000120301     C                   ENDSL
034100120301      **
034200120301     C     Fine__SR      ENDSR
034300120301     C**************************************************************************
034400120301      *  Decodifica Anagrafica dell'AUTISTA
034500120301     C**************************************************************************
034600120301     C     Anagraf_D     begSR
034700120301     c*
034800120301     c     kapd          klist
034900120301     c                   kfld                    TIPO              1
035000120301     c                   kfld                    adtpdr
035100120301     c                   eval       TIPO = 'D'
035200120301      **
035300120301     c     kapd          chain     fiapd01l
035400120301     c                   if        %found(fiapd01l)
035500120301     c                   movel     apdrsf        desPDR
035600120301     c                   end
035700120301      **
035800120301     C                   ENDSR
035900120301     C**************************************************************************
036000120301      *  Controlli x anomalie (A)
036100120301     C**************************************************************************
036200120301     C     contr__A      begSR
036300120301      **
036400120301      *  Se è stata trovata la Partita
036500120301     c                   if        Trazionista = SI
036600120301      *
036700120301     c                   if         adtNRC  = 0
036800120607     c                   eval      note = 'TRAZIONISTA!!  Nr.Contr.=(0)  Tar.'
036900120607     c                             + ' <  di 9000000'
037000120503     c                   leavesr
037100120301     c                   elseIf     adtNRC  > 0
037200120607     c                   eval      note = 'TRAZIONISTA!!  Nr.Contr.su Tariffa'
037300120607     c                             + ' <  di 9000000'
037400120503     c                   leavesr
037500120301     c                   endif
037600120301      **
037700120301     c                   endif
037800120301      **
037900120301     C     end_CtrlA     ENDSR
038000120301     C**************************************************************************
038100120301      *  Controlli x anomalie (B)
038200120301     C**************************************************************************
038300120301     C     contr__B      begSR
038400120301      **
038500120301      *  Se è stata trovata la Partita quindi è un trazionista
038600120301      **    però è SCADUTO --> deve far scadere la Tariffa
038700120301     c                   if        Trazionista = SI
038800120301      *
038900120302     c                   if          ATSDTFIC < data_oggi and ATSDTFIC > 0
039000120509     c                   eval      note = 'TRAZIONISTA Disaccreditato! il '
039100120313     c                             + %editw(ATSDTFIC:datmask)
039200120313     c                             + ' con Tar. ancora ATTIVA'
039300120503     c                   leavesr
039400120301     c                   end
039500120301      *
039600120301     c                   else
039700120301      *
039800120511     c                   if        atsDEC = 0
039900120607     c                   eval      note = 'Tariffa da Trazionista MA NON'
040000120607     c                             + ' è un TRAZIONISTA !! '
040100120503     c                   leavesr
040200120511     c                   else
040300120608     c                   if        atsDFC > 0 and atsDFC < 20391231
040400120607     c                   eval      note = 'Tariffa come TRAZIONISTA ma il co'
040500120607     c                             + 'ntratto da TRAZIONISTA è CHIUSO !!'
040600120511     c                   leavesr
040700120511     c                   endif
040800120511     c                   endif
040900120301      *
041000120301     c                   endif
041100120301      **
041200120301     C     end_CtrlB     ENDSR
041300040825     C**************************************************************************
041400040825      *
041500040825     C**************************************************************************
041600120503     C     FineFile      begSR
041700040826      *
041800040825     c* ROTTURA Fattura
041900120301     c                   if        adtpdr <> savpdr or
042000040825     C                             SqlCod = 100
042100120301     c                   eval      savpdr =  adtpdr
042200040826     c                   end
042300040825      *
042400040825     C                   ENDSR
042500120301     C/EJECT
042600120301     c*-------------------------------------------------------------------------
042700120301     c* tramite Società e Unità decodifica P.IVA su PROJ
042800120301     c*-------------------------------------------------------------------------
042900120301     C     Decod_P_IVA   BegSR
043000120301      **
043100120301      **  Routine x Reperire dati PIVA:
043200120301      **    La routine in base alla sottonatura può funzionare
043300120301      **     x F=Fornitore/C=Cliente
043400120301     C                   clear                   trmz70s4ds                     Input
043500120301     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
043600120301     C                   move      adtCDF        s4_keyKSC                      Input
043700120301     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
043800120301     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
043900120301     c                   if        adtFIL      > 0
044000120301     C                   movel(p)  adtFIL        s4_Unita                       Input/Output
044100120301     c                   end
044200120301     c                   call      'TRMZ70SR4'
044300120301     c                   parm                    trmz70s4ds
044400120301      *
044500120301      *  se torna con l'errore riprova e aveva passato l'unità
044600120301      *   riprova passandogli l'unità vuota
044700120301     c                   if        s4_errore <> '0'
044800120301     C                   clear                   trmz70s4ds                     Input
044900120301     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
045000120301     C                   move      adtCDF        s4_keyKSC                      Input
045100120301     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
045200120301     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
045300120301     c                   call      'TRMZ70SR4'
045400120301     c                   parm                    trmz70s4ds
045500120301     c                   end
045600120301      *
045700120301      *  Descrizione Ragione Sociale FORNITORE
045800120301     c                   if        s4_errore = '0'
045900120301     c                   eval        DESFRN = s4_RAGSOCKSC
046000120301     c                   eval        PIVA   = s4_PARTITAIVA
046100120301     c                   end
046200120301      *
046300120301     C                   ENDSR
046400120301     C/EJECT
046500120301     c*-------------------------------------------------------------------------
046600120313      **   se Disaccreditato
046700120301     c*-------------------------------------------------------------------------
046800120313     c     se_DISACCRED  begsr
046900120301      **
047000120313      **  trova data disaccreditamento se NON è un trazionista
047100120313     c     adtPDR        setll     aitra05l
047200120313     c     adtPDR        reade     aitra05l
047300120313     c                   dow       not %EoF(Aitra05l)
047400120313      *
047500120313      *  se attivo -> fuori
047600120313     c                   if        tradfi = 0
047700120313     c                   leave
047800120313     c                   end
047900120313      *  imposta la data
048000120313     C                   move      *zeros        G02DAT
048100120313     C                   MOVE      traDFI        G02INV
048200120313     C                   MOVE      '3'           G02ERR
048300120313     C                   CALL      'XSRDA8'
048400120313     C                   PARM                    WLBDAT
048500120313     C                   Z-ADD     G02dat        datDIS
048600120313      *
048700120313     c     adtPDR        reade     aitra05l
048800120313     c                   enddo
048900120313     ***
049000120313     C                   ENDSR
049100120313     C/EJECT
049200120313     c*-------------------------------------------------------------------------
049300120313      **   Ultima Valorizzazione
049400120313     c*-------------------------------------------------------------------------
049500120313     c     Ultima_VALOR  begsr
049600120313      **
049700120313      * Data Ultima valorizzazione
049800120313     ***
049900120313     **  Aggancio l'ultima valorizzazione fatta dall'AUTISTA
050000120313     C/EXEC SQL
050100120314     C+ SELECT     ifNULL(max(ATTDDC) , 0) into :datUVA
050200120313     C+  FROM   FIATT00F WHERE
050300120313     C+  ATTPDR  = :adtPDR
050400120313     C/END-EXEC
050500120313      *  imposta la data
050600120313     c                   if        datUVA > 0
050700120313     C                   move      *zeros        G02DAT
050800120313     C                   MOVE      datUVA        G02INV
050900120313     C                   MOVE      '3'           G02ERR
051000120313     C                   CALL      'XSRDA8'
051100120313     C                   PARM                    WLBDAT
051200120313     C                   Z-ADD     G02dat        datUVA
051300120313     c                   end
051400120313     ***
051500120313     C                   ENDSR
051600120313     C/EJECT
051700120313     c*-------------------------------------------------------------------------
051800120313      **   esegue un controllo tramite la PARTITA IVA
051900120313     c*-------------------------------------------------------------------------
052000120313     c     se_TRAZIONISTAbegsr
052100120313      **
052200120301     c                   eval      trazionista = NO
052300120301      **
052400120301      **  controlla se la P.IVA esiste sul file dei Trazionisti
052500120301     c                   clear                   partraz
052600120503     c                   eval      pardtin = adtddt
052700120518     c                   movel     adtcdf        parsoc
052800120503     c                   eval      pardtfi = adtdst
052900120301     c                   movel     s4_PARTITAIVA parivaT
053000120301     c                   call      'TRULTRAZ'
053100120301     c                   parm                    partraz
053200120306     c                   parm                    dsAIATS
053300120301      *
053400120717     c                   if         esitoT <> ' '
053500120306     c                   eval      trazionista = SI
053600120508     c                   else
053700120508      * controlla comunque se esiste un record con Partita IVA
053800120508      *       sui contratti
053900120508     C/EXEC SQL
054000120508     C+ SELECT  aiats00f.* into :dsAIATS
054100120508     C+  FROM   AIATS00F
054200120508     C+  WHERE  ATSPIVA = :s4_PARTITAIVA
054300120508     C/END-EXEC
054400120508     c                   if        sqlcod <> 0
054500120508     c                   clear                   dsAIATS
054600120508     c                   end
054700120508      *
054800120301     c                   end
054900120301      *
055000120301     C                   ENDSR
055100120301     C/EJECT
055200120301     c*-------------------------------------------------------------------------
