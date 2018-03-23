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
002900120808     D ficn27ds      e DS
003000120301     D*
003100120301     D trmz70s4ds    E DS                  prefix(S4_)
003200120301
003300120502     D partraz         DS
003400120518     D  parsoc                        3s 0
003500120502     D  parivat                      16
003600120502     D  pardtin                       8s 0
003700120502     D  pardtfi                       8s 0
003800120502     d  esitot                        1
003900120502     d  errmsg                       75
004000120301
004100040820     D WLBDAT          DS
004200040820     D  G02DAT                 1      8  0
004300040820     D  G02INV                 9     16  0
004400040820     D  G02ERR                17     17
004500040820     D  G02TGI                18     22  0
004600040820      *
004700120301     D  savpdr         s                   like(adtpdr)
004800120301      *
004900040820     D WrkSqlCmd       S           1024
005000120301      *
005100040820     D  data_Oggi      s                   like(G02INV) INZ(0)
005200120301     D Trazionista     s              2
005300040820      *
005400120301     D SI              C                   CONST('SI')
005500120301     D NO              C                   CONST('NO')
005600040820     D digits          C                   CONST('0123456789')
005700120313     D datmask         C                   CONST('    /  / 0')
005800040820      *****************************************************************
005900120301      **
006000120301     c* compongo istruzione sql per il TEST (A)
006100120301     c                   exsr      istruzA
006200120301     c                   exsr      Exec__A
006300120301      **
006400120301     c* compongo istruzione sql per il TEST (B)
006500120301     c                   exsr      istruzB
006600120301     c                   exsr      Exec__B
006700120301      ***
006800020326     C                   SETON                                        LR
006900040820     C**************************************************************************
007000040820     C* INIZIALIZZAZIONE
007100040820     C**************************************************************************
007200040820     C     *inzsr        BEGSR
007300040820      **
007400040820     C     *ENTRY        PLIST
007500040820     C                   PARM                    KPJBA
007600120808     c                   movel     kpjbu         ficn27ds
007700120808     c                   movel     d27solor      soloano           1
007800041103      *
007900041103     C                   Z-ADD     1             CODUT
008000041103     C                   CALL      'X§PARUT'
008100041103     C                   PARM                    UT§DSE
008200041103     C                   MOVEL     REC80         CNCR80
008300041103     C                   MOVEL     RAGUT         RSUT             20
008400040820      *
008500040820      *  imposta UDATE di elaborazione
008600040820     C                   move      *date         G02DAT
008700040820     C                   MOVE      *ZEROS        G02INV
008800040820     C                   MOVE      *BLANKS       G02ERR
008900040820     C                   CALL      'XSRDA8'
009000040820     C                   PARM                    WLBDAT
009100040820     C                   Z-ADD     G02INV        data_oggi
009200120302     C                   move      *date         day
009300040820     C                   TIME                    W0140            14 0
009400040820     C                   MOVEl     W0140         ore
009500040823      **
009600040823     c                   seton                                        66
009700040825     c                   clear                   prima_volta       1
009800120301     c                   clear                   almeno_uno        1
009900040820     c*
010000040820     C                   ENDSR
010100040820     C**************************************************************************
010200120301     C* Compone l'istruzione sql x il test (A)
010300040820     C**************************************************************************
010400120301     C     istruzA       BEGSR                                                  *
010500040820      **
010600120301     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file FIADT (tariffe affl/defl)
010700040820     C                   EVAL      WrkSqlCmd
010800040820     C                             =
010900120301     C                             'SELECT * from FIADT00F where ADTNRC '
011000120607     c                              + ' < '
011100120607     c                              + ' 9000000 and ADTDST >= '
011200120301     c                              + %trim(%char(data_oggi))
011300120301     c                              + ' ORDER BY ADTPDR '
011400120607      **
011500040820     C                   ENDSR
011600120301     C**************************************************************************
011700120301     C* esegue l'istruzione sql per il TEST (A)
011800120301     C**************************************************************************
011900120301     C     Exec__A       BEGSR                                                  *
012000120301      **
012100120301     c                   move      'A'           RoutineAB         1
012200120301     C/EXEC SQL
012300120301     C+ PREPARE S1 FROM :WrkSqlCmd
012400120301     C/END-EXEC
012500120301
012600120301     C/EXEC SQL
012700120301     C+ DECLARE A1 CURSOR FOR S1
012800120301     C/END-EXEC
012900120301
013000120301     C/EXEC SQL
013100120301     C+ OPEN A1
013200120301     C/END-EXEC
013300120301     C                   DOU       SqlCod <> 0
013400120301     c*  cerca le tariffe con Nr.Contratto <> 9999999 per verificare
013500120301     c*   (tramite la Partita IVA) se trattasi di TRAZIONISTI.
013600120301     c*  Qui si possono trovare 2 casi:
013700120301     c*     1) il nr.contratto = 0 ->  quindi deve essere impostato con 9999999
013800120301     c*     2) il nr.contratto > 0 ->  quindi è legato ad un contratto di città
013900120301     C/EXEC SQL
014000120301     C+ FETCH NEXT FROM A1 INTO :fiadt00f
014100120301     C/END-EXEC
014200120808      * filtra per filiale se immessa
014300120808     c                   movel     adtpdr        filpadr           3 0
014400120808     c                   if        d27fil <> 0 and
014500120808     c                             d27fil <> filpadr
014600120808     c                   iter
014700120808     c                   endif
014800120808      *
014900120301     c                   exsr      Exec_AB
015000120301      ***
015100120301      **  x FineFile o errore
015200120301     C                   if        SqlCod = 100 or *inH1 = *ON
015300120301     c                   leave
015400120301     c                   end
015500120301      ***
015600120301     C                   ENDDO
015700120301     C/EXEC SQL
015800120301     C+ CLOSE A1
015900120301     C/END-EXEC
016000120301      **
016100120301     C                   ENDSR
016200120301     C**************************************************************************
016300120301     C* Compone l'istruzione sql x il test (B)
016400120301     C**************************************************************************
016500120301     C     istruzB       BEGSR                                                  *
016600120301      **
016700120301     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file FIADT (tariffe affl/defl)
016800120301     C                   EVAL      WrkSqlCmd
016900120301     C                             =
017000120301     C                             'SELECT * from FIADT00F where ADTNRC '
017100120607     c                              + ' >= '
017200120607     c                              + ' 9000000 and ADTDST >= '
017300120301     c                              + %trim(%char(data_oggi))
017400120301     c                              + ' ORDER BY ADTPDR '
017500120607      **
017600120301     C                   ENDSR
017700120301     C**************************************************************************
017800120301     C* esegue l'istruzione sql per il TEST (B)
017900120301     C**************************************************************************
018000120301     C     Exec__B       BEGSR                                                  *
018100120301      **
018200120301     c                   move      'B'           RoutineAB
018300120301     C/EXEC SQL
018400120301     C+ PREPARE S2 FROM :WrkSqlCmd
018500120301     C/END-EXEC
018600120301
018700120301     C/EXEC SQL
018800120301     C+ DECLARE B1 CURSOR FOR S2
018900120301     C/END-EXEC
019000120301
019100120301     C/EXEC SQL
019200120301     C+ OPEN B1
019300120301     C/END-EXEC
019400120301     C                   DOU       SqlCod <> 0
019500120301     c*  cerca le tariffe con Nr.Contratto = 9999999 per verificare
019600120301     c*   (tramite la Partita IVA)
019700120301     c*     1) se NON trattasi di TRAZIONISTA
019800120301     c*     2) se il trazionista ha o aveva già la data di FINE rapporto.
019900120301     C/EXEC SQL
020000120301     C+ FETCH NEXT FROM B1 INTO :fiadt00f
020100120301     C/END-EXEC
020200120808      * filtra per filiale se immessa
020300120808     c                   movel     adtpdr        filpadr           3 0
020400120808     c                   if        d27fil <> 0 and
020500120808     c                             d27fil <> filpadr
020600120808     c                   iter
020700120808     c                   endif
020800120808      *
020900120301     c                   exsr      Exec_AB
021000120301      ***
021100120301      **  x FineFile o errore
021200120301     C                   if        SqlCod = 100 or *inH1 = *ON
021300120301     c                   leave
021400120301     c                   end
021500120301      ***
021600120301     C                   ENDDO
021700120301     C/EXEC SQL
021800120301     C+ CLOSE B1
021900120301     C/END-EXEC
022000120301      **
022100120301     C                   ENDSR
022200120301     C**************************************************************************
022300120301     C* esegue l'istruzione sql per il TEST (A)
022400120301     C**************************************************************************
022500120301     C     Exec_AB       BEGSR                                                  *
022600120301      **
022700120301     C                   SELECT
022800120301     c*
022900120301     c* a EOF o x rotture di livello
023000120301     C                   WHEN      SqlCod = 100
023100120301      *
023200120301     c                   if        almeno_uno ='S'
023300120503     c                   exsr      FineFile
023400120301     c                   else
023500120301      *
023600120504     c                   if        RoutineAB = 'B'
023700120504     c                   if        *in66
023800120504      *                 * ------------------ *
023900120301     c                   write     testa
024000120301     c                   write     tesdetta
024100120504      *                 * ------------------ *
024200120504     c                   setoff                                       66
024300120504     c                   else
024400120504     c                   seton                                        66
024500120504     c                   end
024600120504      **
024700120301     c                   write     NODATI
024800120301     c                   end
024900120504     c                   end
025000120301      *
025100120503     c                   leavesr
025200120301     **
025300120301     C                   WHEN      SqlCod < 0
025400120301     C                   seton                                        H1
025500120503     c                   leavesr
025600120301     c*
025700120301     C                   OTHER
025800120301      * DETTAGLIO
025900120301     c                   if        *in66
026000120301      *                 * ------------------ *
026100120301     c                   write     testa
026200120301     c                   write     tesdetta
026300120301      *                 * ------------------ *
026400120301     c                   setoff                                       66
026500120301     c                   eval      prima_volta = 'N'
026600120301     c                   end
026700120301      *
026800120301      **  pulisce le descrizioni da Decodificare
026900120301     c                   eval        DESFRN = *blank
027000120301     c                   eval        PIVA   = *blank
027100120301     c                   eval        DESPDR = *blank
027200120301     c                   eval        NOTE   = *blank
027300120313     c                   clear                   datDIS
027400120313     c                   clear                   datUVA
027500120301      *
027600120301      * ricava la Partita IVA
027700120301     c                   exsr      Decod_P_IVA
027800120301      *
027900120301      **  con P.IVA controlla se è un trazionista
028000120301     c                   exsr      se_TRAZIONISTA
028100120301      *
028200120313      **  se non è un trazionista controlla se DIsaccreditato
028300120508     c                   clear                   DATTACC
028400120508     c                   clear                   DATTDIS
028500120508     c                   clear                   DATTNRC
028600120511     c                   clear                   DATTDFC
028700120508      *
028800120313     c                   if        Trazionista = NO
028900120313     c                   exsr      se_DISACCRED
029000120508     c                   else
029100120508      *
029200120508     c                   end
029300120508      * date TRAINI
029400120508      *
029500120508     C                   move      *zeros        G02DAT
029600120508     C                   MOVE      atsDTINC      G02INV
029700120508     C                   MOVE      '3'           G02ERR
029800120508     C                   CALL      'XSRDA8'
029900120508     C                   PARM                    WLBDAT
030000120508     C                   Z-ADD     G02dat        datTACC
030100120508      *
030200120508     C                   move      *zeros        G02DAT
030300120508     C                   MOVE      atsDTFIC      G02INV
030400120508     C                   MOVE      '3'           G02ERR
030500120508     C                   CALL      'XSRDA8'
030600120508     C                   PARM                    WLBDAT
030700120508     C                   Z-ADD     G02dat        datTDIS
030800120508      *
030900120508     C                   move      *zeros        G02DAT
031000120508     C                   MOVE      atsDEC        G02INV
031100120508     C                   MOVE      '3'           G02ERR
031200120508     C                   CALL      'XSRDA8'
031300120508     C                   PARM                    WLBDAT
031400120508     C                   Z-ADD     G02dat        datTNRC
031500120511      *
031600120511     C                   move      *zeros        G02DAT
031700120511     C                   MOVE      atsDFC        G02INV
031800120511     C                   MOVE      '3'           G02ERR
031900120511     C                   CALL      'XSRDA8'
032000120511     C                   PARM                    WLBDAT
032100120511     C                   Z-ADD     G02dat        datTDFC
032200120313      *
032300120313      * Ultima Vlaorizzazione
032400120313     c                   exsr      Ultima_VALOR
032500120313      *
032600120301      *     Controlli specifici
032700120301     c                   if        RoutineAB = 'A'
032800120301     c                   exsr      contr__A
032900120301     c                   else
033000120301     c                   exsr      contr__B
033100120301     c                   end
033200120301      *
033300120301      *  scrive la riga di segnalazione
033400120301     c                   if        NOTE <> *BLANKs
033500120301      *
033600120301      *      Decodifica l'Autista della Tariffa
033700120301     c                   exsr      Anagraf_D
033800120301      *
033900120302     C                   move      *zeros        G02DAT
034000120302     C                   MOVE      adtDDT        G02INV
034100120302     C                   MOVE      '3'           G02ERR
034200120302     C                   CALL      'XSRDA8'
034300120302     C                   PARM                    WLBDAT
034400120302     C                   Z-ADD     G02dat        adtDDT
034500120302      *
034600120302     C                   move      *zeros        G02DAT
034700120302     C                   MOVE      adtDST        G02INV
034800120302     C                   MOVE      '3'           G02ERR
034900120302     C                   CALL      'XSRDA8'
035000120302     C                   PARM                    WLBDAT
035100120302     C                   Z-ADD     G02dat        adtDST
035200120302      *
035300120301     c                   write     dettagli
035400120301     c                   move      'S'           almeno_uno
035500120301     c                   end
035600120301     c*
035700120301     C                   ENDSL
035800120301      **
035900120301     C     Fine__SR      ENDSR
036000120301     C**************************************************************************
036100120301      *  Decodifica Anagrafica dell'AUTISTA
036200120301     C**************************************************************************
036300120301     C     Anagraf_D     begSR
036400120301     c*
036500120301     c     kapd          klist
036600120301     c                   kfld                    TIPO              1
036700120301     c                   kfld                    adtpdr
036800120301     c                   eval       TIPO = 'D'
036900120301      **
037000120301     c     kapd          chain     fiapd01l
037100120301     c                   if        %found(fiapd01l)
037200120301     c                   movel     apdrsf        desPDR
037300120301     c                   end
037400120301      **
037500120301     C                   ENDSR
037600120301     C**************************************************************************
037700120301      *  Controlli x anomalie (A)
037800120301     C**************************************************************************
037900120301     C     contr__A      begSR
038000120301      **
038100120301      *  Se è stata trovata la Partita
038200120301     c                   if        Trazionista = SI
038300120301      *
038400120301     c                   if         adtNRC  = 0
038500120925     c                   eval      note = 'TRAZIONISTA Verificare fornitore e'
038600120925     c                             + ' contratto'
038700120503     c                   leavesr
038800120301     c                   elseIf     adtNRC  > 0
038900120925     c                   eval      note = 'TRAZIONISTA abbinato a contratt'
039000120925     c                             + 'o di città '
039100120503     c                   leavesr
039200120301     c                   endif
039300120301      **
039400120301     c                   endif
039500120301      **
039600120301     C     end_CtrlA     ENDSR
039700120301     C**************************************************************************
039800120301      *  Controlli x anomalie (B)
039900120301     C**************************************************************************
040000120301     C     contr__B      begSR
040100120301      **
040200120301      *  Se è stata trovata la Partita quindi è un trazionista
040300120301      **    però è SCADUTO --> deve far scadere la Tariffa
040400120301     c                   if        Trazionista = SI
040500120301      *
040600120302     c                   if          ATSDTFIC < data_oggi and ATSDTFIC > 0
040700120509     c                   eval      note = 'TRAZIONISTA Disaccreditato! il '
040800120313     c                             + %editw(ATSDTFIC:datmask)
040900120313     c                             + ' con Tar. ancora ATTIVA'
041000120503     c                   leavesr
041100120301     c                   end
041200120301      *
041300120301     c                   else
041400120301      *
041500120511     c                   if        atsDEC = 0
041600120607     c                   eval      note = 'Tariffa da Trazionista MA NON'
041700120607     c                             + ' è un TRAZIONISTA !! '
041800120503     c                   leavesr
041900120511     c                   else
042000120608     c                   if        atsDFC > 0 and atsDFC < 20391231
042100120607     c                   eval      note = 'Tariffa come TRAZIONISTA ma il co'
042200120607     c                             + 'ntratto da TRAZIONISTA è CHIUSO !!'
042300120511     c                   leavesr
042400120511     c                   endif
042500120511     c                   endif
042600120301      *
042700120301     c                   endif
042800120301      **
042900120301     C     end_CtrlB     ENDSR
043000040825     C**************************************************************************
043100040825      *
043200040825     C**************************************************************************
043300120503     C     FineFile      begSR
043400040826      *
043500040825     c* ROTTURA Fattura
043600120301     c                   if        adtpdr <> savpdr or
043700040825     C                             SqlCod = 100
043800120301     c                   eval      savpdr =  adtpdr
043900040826     c                   end
044000040825      *
044100040825     C                   ENDSR
044200120301     C/EJECT
044300120301     c*-------------------------------------------------------------------------
044400120301     c* tramite Società e Unità decodifica P.IVA su PROJ
044500120301     c*-------------------------------------------------------------------------
044600120301     C     Decod_P_IVA   BegSR
044700120301      **
044800120301      **  Routine x Reperire dati PIVA:
044900120301      **    La routine in base alla sottonatura può funzionare
045000120301      **     x F=Fornitore/C=Cliente
045100120301     C                   clear                   trmz70s4ds                     Input
045200120301     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
045300120301     C                   move      adtCDF        s4_keyKSC                      Input
045400120301     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
045500120301     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
045600120301     c                   if        adtFIL      > 0
045700120301     C                   movel(p)  adtFIL        s4_Unita                       Input/Output
045800120301     c                   end
045900120301     c                   call      'TRMZ70SR4'
046000120301     c                   parm                    trmz70s4ds
046100120301      *
046200120301      *  se torna con l'errore riprova e aveva passato l'unità
046300120301      *   riprova passandogli l'unità vuota
046400120301     c                   if        s4_errore <> '0'
046500120301     C                   clear                   trmz70s4ds                     Input
046600120301     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
046700120301     C                   move      adtCDF        s4_keyKSC                      Input
046800120301     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
046900120301     C                   movel(p)  adtCSF        s4_Societa                     Input/Output
047000120301     c                   call      'TRMZ70SR4'
047100120301     c                   parm                    trmz70s4ds
047200120301     c                   end
047300120301      *
047400120301      *  Descrizione Ragione Sociale FORNITORE
047500120301     c                   if        s4_errore = '0'
047600120301     c                   eval        DESFRN = s4_RAGSOCKSC
047700120301     c                   eval        PIVA   = s4_PARTITAIVA
047800120301     c                   end
047900120301      *
048000120301     C                   ENDSR
048100120301     C/EJECT
048200120301     c*-------------------------------------------------------------------------
048300120313      **   se Disaccreditato
048400120301     c*-------------------------------------------------------------------------
048500120313     c     se_DISACCRED  begsr
048600120301      **
048700120313      **  trova data disaccreditamento se NON è un trazionista
048800120313     c     adtPDR        setll     aitra05l
048900120313     c     adtPDR        reade     aitra05l
049000120313     c                   dow       not %EoF(Aitra05l)
049100120313      *
049200120313      *  se attivo -> fuori
049300120313     c                   if        tradfi = 0
049400120313     c                   leave
049500120313     c                   end
049600120313      *  imposta la data
049700120313     C                   move      *zeros        G02DAT
049800120313     C                   MOVE      traDFI        G02INV
049900120313     C                   MOVE      '3'           G02ERR
050000120313     C                   CALL      'XSRDA8'
050100120313     C                   PARM                    WLBDAT
050200120313     C                   Z-ADD     G02dat        datDIS
050300120313      *
050400120313     c     adtPDR        reade     aitra05l
050500120313     c                   enddo
050600120313     ***
050700120313     C                   ENDSR
050800120313     C/EJECT
050900120313     c*-------------------------------------------------------------------------
051000120313      **   Ultima Valorizzazione
051100120313     c*-------------------------------------------------------------------------
051200120313     c     Ultima_VALOR  begsr
051300120313      **
051400120313      * Data Ultima valorizzazione
051500120313     ***
051600120313     **  Aggancio l'ultima valorizzazione fatta dall'AUTISTA
051700120313     C/EXEC SQL
051800120314     C+ SELECT     ifNULL(max(ATTDDC) , 0) into :datUVA
051900120313     C+  FROM   FIATT00F WHERE
052000120313     C+  ATTPDR  = :adtPDR
052100120313     C/END-EXEC
052200120313      *  imposta la data
052300120313     c                   if        datUVA > 0
052400120313     C                   move      *zeros        G02DAT
052500120313     C                   MOVE      datUVA        G02INV
052600120313     C                   MOVE      '3'           G02ERR
052700120313     C                   CALL      'XSRDA8'
052800120313     C                   PARM                    WLBDAT
052900120313     C                   Z-ADD     G02dat        datUVA
053000120313     c                   end
053100120313     ***
053200120313     C                   ENDSR
053300120313     C/EJECT
053400120313     c*-------------------------------------------------------------------------
053500120313      **   esegue un controllo tramite la PARTITA IVA
053600120313     c*-------------------------------------------------------------------------
053700120313     c     se_TRAZIONISTAbegsr
053800120313      **
053900120301     c                   eval      trazionista = NO
054000120301      **
054100120301      **  controlla se la P.IVA esiste sul file dei Trazionisti
054200120301     c                   clear                   partraz
054300120503     c                   eval      pardtin = adtddt
054400120518     c                   movel     adtcdf        parsoc
054500120503     c                   eval      pardtfi = adtdst
054600120301     c                   movel     s4_PARTITAIVA parivaT
054700120301     c                   call      'TRULTRAZ'
054800120301     c                   parm                    partraz
054900120306     c                   parm                    dsAIATS
055000120301      *
055100120717     c                   if         esitoT <> ' '
055200120306     c                   eval      trazionista = SI
055300120508     c                   else
055400120508      * controlla comunque se esiste un record con Partita IVA
055500120508      *       sui contratti
055600120508     C/EXEC SQL
055700120508     C+ SELECT  aiats00f.* into :dsAIATS
055800120508     C+  FROM   AIATS00F
055900120508     C+  WHERE  ATSPIVA = :s4_PARTITAIVA
056000120508     C/END-EXEC
056100120508     c                   if        sqlcod <> 0
056200120508     c                   clear                   dsAIATS
056300120508     c                   end
056400120508      *
056500120301     c                   end
056600120301      *
056700120301     C                   ENDSR
056800120301     C/EJECT
056900120301     c*-------------------------------------------------------------------------
