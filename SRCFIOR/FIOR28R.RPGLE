000100011023     H DECEDIT('0,') DATEDIT(*DMY.)
000200011023
000300011023      ****************************************************************
000400011023      *                                                              *
000500011030      *     ? INTERROGAZIONE VAS O.R.M. DA CLIENTI VIA INTERNET ?    *
000600011023      *                                                              *
000700011023      ****************************************************************
000800011023
000900011024     FTIORI01L  IF   E           K DISK    rename(TIORI000:tiori01)
001000120523     f                                     usropn
001100120523     f                                     extfile(wLib)
001200011023     FTIORI02L  IF   E           K DISK    rename(TIORI000:tiori02)
001300120523     f                                     usropn
001400120523     f                                     extfile(wLib)
001500011023     FTIORI03L  IF   E           K DISK    rename(TIORI000:tiori03)
001600120523     f                                     usropn
001700120523     f                                     extfile(wLib)
001800011023     FTIORI04L  IF   E           K DISK    rename(TIORI000:tiori04)
001900120523     f                                     usropn
002000120523     f                                     extfile(wLib)
002100011107     FAZORG01L  IF   E           K DISK
002200011024     FCNACO00F  IF   E           K DISK
002300011025     FFNACR01L  IF   E           K DISK
002400011024     FFIOR28D   CF   E             WORKSTN sfile(fior28S:nrr1)
002500011024     F                                     infds(DSFMT)
002600011023
002700011023      *------------------------------------------------------------------------*
002800011024
002900011024      *   S C H I E R E
003000011024
003100011102     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             MSG VIDEO
003200011024      *
003300011024     D L6              S              3  0 DIM(30)                              P.O. Gestiti
003400011024
003500011024      *   V A R I A B I L I
003600011024
003700011024      *  P.O. in gestione
003800011024     D wFGS            S              3  0 INZ
003900011025      *  ordinamento selezionato
004000011025     D wORD            S              2  0 INZ
004100011025      *  campi data in formato AAAAMMGG
004200011024     D w1dida          S                   like(ORIder) INZ
004300011024     D w1dia           S                   like(ORIder) INZ
004400011025      *  campi ora in formato HHMMSS
004500011029     D w1time          S              6  0 INZ
004600011025     D w1oida          S                   like(ORIoer) INZ
004700011025     D w1oia           S                   like(ORIoer) INZ
004800011025      *  indici di schiera
004900011024     D X               S              2  0 INZ
005000011029     D XX              S              2  0 INZ
005100011025      *  flag
005200011024     D OK_rec          S              1    inz(*off)
005300011025      *  dati per la gestione del subfile
005400011024     D Nrr1            S                   like(Recsf) INZ
005500011025      *  pagina fino a cui deve essere ricaricato il sfl
005600011024     D WPag            S                   like(Recsf) INZ
005700011024     D WMax            S                   like(Recsf) INZ
005800011025     D Pagine          S              4  0 INZ
005900011025     D Resto           S              3  0 INZ
006000011025      *  campi di comodo
006100011030     D wTest           S             40    inz
006200011031     D wSize1          S              3  0 inz
006300011109     D wSize2          S              3  0 inz
006400011109     D wSize3          S              3  0 inz
006500080722
006600080722     d codut           s                   like(acokut) inz(1)
006700080722     d kci             s                   like(acokcc) inz(151)
006800120523
006900120523       // - Nome libreria dei file TIORI
007000120523     d wLib            s             21    inz
007100011024
007200011025      * nr. di rercor per pagina del subfile
007300011025     D SFLPAG          C                   CONST(16)
007400011025      * Caratteri numerici
007500011025     D DIGITn          C                   CONST('1234567890')
007600011024      * titolo videata (lunghezza massima 34)
007700011030     D TIT_A           C                   CONST('*** INTERROGAZIONE VAS  O.R.M-
007800011030     D                                     . ***')
007900011024      * Descrizione opzioni (lunghezza massima 78)
008000011030     D DOPZ1           C                   CONST('5=Visualizza VAS   9=Visualiz-
008100011030     D                                     za ORM                              -
008200011024     D                                             ')
008300011023
008400011023      *   D S   I N T E R N E / E S T E R N E
008500011024
008600011024     D DSFMT           DS           512
008700011025     D  TASTO                369    369
008800011024     D  SFLNRR               378    379B 0
008900011024
009000011024     D WLBDAT          DS                  INZ
009100011024     D  G02DAT                 1      8  0
009200011024     D  G02INV                 9     16  0
009300011024     D  G02ERR                17     17
009400011024     D  G02TGI                18     22  0
009500011023
009600011024     D FIOR07DS        DS                  INZ
009700011023     D  ppoe                          3  0
009800011023     D  pnor                          7  0
009900011023     D  pnsr                          2  0
010000011023     D  pnrv                          2  0
010100011023     D  psce                          1
010200011023     D  pfgs                          3  0
010300011023     D  ppor                          3  0
010400011023     D  pdtndc                        8  0
010500011023     D  pmdo                         10
010600011023     D  ppro                         10
010700011023     D  pdtrist                       8  0
010800011023     D  prmp                          1
010900011023     D  pbrc                          1
011000011023     D  pref                          2
011100011023     D  pmio                          1
011200011023     D  pgas                          1
011300011023     D  pndc                          7  0
011400011023     D  ptpela                        1
011500011030      *
011600011030     D FIOR29DS        DS                  INZ
011700011030     D  or29err                       1
011800011106     D  or29end                       1
011900011031     D  or29prg                       7  0
012000011031     D  or29poe                       3  0
012100011031     D  or29nsr                       2  0
012200011031     D  or29nor                       7  0
012300011031     D  or29nrv                       2  0
012400011030      *
012500011030     D                 DS
012600011030     D  v1cra1
012700011030     D  v1cra2
012800011030     D  v1cra3
012900011030     D  v1cra                  1     10  0
013000011030      *
013100011030     D                 DS
013200011030     D  v1crc1
013300011030     D  v1crc2
013400011030     D  v1crc3
013500011030     D  v1crc                  1     10  0
013600011024      *
013700011024     D TRUL06DS      E DS
013800080722     D  LIN                    1     90  0 DIM(30)
013900011030      *
014000131216     d FIOR81ds      e ds                  inz
014100011024      *
014200080722      * - Parametri x Controllo profilo utenti
014300080722     d TIBS34ds      e ds
014400080722      * - Ds di riferimento al file esterno AZUTE00F
014500080722     d AZUTEds       e ds                  extname(AZUTE00F)
014600080722      * - Ds per dati organigramma
014700080722     d DDatiUte      e ds
014800011023      *
014900011023     D KPJBA         E DS
015000011023      *
015100011023     D                SDS
015200011024     D  VTCPGM           *PROC
015300011024     D  PARMS            *PARMS
015400011023
015500011023      *------------------------------------------------------------------------*
015600011024
015700011024      ****************************************************************
015800011024      *  RIEPILOGO INDICATORI
015900011024      ****************************************************************
016000011024      * 10 - COMODO
016100011024      * 12 - SONO IN SEDE
016200011024      * 21 - SELEZIONE CODICE: CLIENTI
016300011024      * 22 - SELEZIONE CODICE: FORNITORI
016400011024      * 23 - SELEZIONE CODICE: AGENTI
016500011024      * 24 - SELEZIONE CODICE: ESTERO
016600011024      * 25 - SELEZIONE CODICE: ITALIA
016700011024      * 26 - SELEZIONE CODICE: CAPO CONTO IVA
016800011024      * 27 - SELEZIONE CODICE: ? ? ? ? ?
016900011025      * 28 - ERRORE GENERICO DSPF
017000011106      * 29 - ALTRA CONDIZIONE PER USCIRE DAL CICLO...
017100011024      * 30 - GESTIONE SUBFILE: SFLDSP
017200011024      * 31 - GESTIONE SUBFILE: SFLDSPCTL
017300011024      *N31 - GESTIONE SUBFILE: SFLCLR
017400011024      * 32 - GESTIONE SUBFILE: SFLNXTCHG
017500011024      * 33 - GESTIONE SUBFILE: SFLEND
017600011024      * 40 - ERRORE: Effettuare almeno una selezione
017700011026      * 41 - ERRORE: Data immissione dal errata
017800011026      * 42 - ERRORE: Orario immissione dalle errato
017900011026      * 43 - ERRORE: Data immissione al  errata
018000011026      * 44 - ERRORE: Orario immissione alle  errato
018100011024      * 45 - ERRORE: Codice cliente errato
018200011030      * 46 - ERRORE: Riferimento O.R.M. (anche parziale) obbligatorio
018300011107      * 47 - ERRORE: Codice P.O. emittente errato
018400011107      * 48 - ERRORE: Codice cliente mittente errato
018500011107      * 49 - ERRORE: Ragione sociale mittente (anche parziale) obbligatoria
018600011107      * 50 - ERRORE: Codice cliente destinatario errato
018700011107      * 51 - ERRORE: Ragione sociale destinatario (anche parziale) obbligatoria
018800011030      * 80 - ERRORE: O.R.M. non confermato
018900011025      * 89 - OPZIONE NON CONSENTITA
019000011026      * 90 - RIEMISSIONE VIDEATA
019100011031      * 91 - NON INIZIALIZZARE LA 1.a VIDEATA
019200011024      ****************************************************************
019300011024
019400011024      * Gestione videate
019500011024
019600011024DO  1C                   do        *hival
019700011029
019800011029      *  Pulizia 1° Videata
019900011030IF  2C                   if        *in28 = *off and *in90 = *off
020000011031     C                                          and *in91 = *off
020100011029     C                   exsr      Sr_Pulvid1
020200011030E   2C                   endif
020300011023
020400011024      *  Emissione 1° Videata
020500011023     C                   exfmt     fior281
020600011026     C                   setoff                                       2890
020700011112     C                   setoff                                       404142
020800011112     C                   setoff                                       434445
020900011112     C                   setoff                                       464748
021000011112     C                   setoff                                       495051
021100011030     C                   clear                   v1cmsg
021200011023
021300011024      *  F3=Fine
021400011023     C   kc              leave
021500011030
021600011030      *  F5=Int.PDC
021700011030     C   ke              exsr      Sr_RicPDC
021800011030
021900011030      *  F7=Int.Cod.Ritiro
022000011030     C   kg              exsr      Sr_RicRitiro
022100011030
022200011030      *  F9=Int.Cod.Ritiro
022300011112     C   ki              exsr      Sr_RicDestin
022400011112
022500011112     C   90              iter
022600011023
022700011024      *  Controlli 1° videata
022800011023     C                   exsr      Sr_Contr01
022900011023
023000011025     C   28
023100011025     Cor 90              iter
023200011023
023300011024      *  Imposto opzioni
023400011023     C                   exsr      Sr_Caropz
023500011023
023600011024      *  Gestione subfile
023700011023     C                   exsr      Sr_Gessfl
023800011024IF  2C                   if        *inkc = *on
023900011023     C                   leave
024000011024E   2C                   endif
024100011023
024200011024E   1C                   enddo
024300011023
024400011023     C                   eval      *inlr = *on
024500011024
024600011023      *****************************************************************
024700011023      * PULIZIA 1° VIDEATA
024800011023      *****************************************************************
024900011023     C     Sr_Pulvid1    BEGSR
025000011023
025100011024     C                   clear                   v1dida
025200011024     C                   clear                   v1oida
025300011024     C                   clear                   v1dia
025400011024     C                   clear                   v1oia
025500011024     C                   clear                   v1idc
025600011024     C                   clear                   v1rags
025700011030     C                   clear                   v1noid
025800011024     C                   clear                   v1rfa
025900011030     C                   clear                   v1rxs1
026000011107     C                   clear                   v1noco
026100011107     C                   clear                   v1cpoe
026200011107     C                   clear                   v1dpoe
026300011030     C                   clear                   v1cra
026400011030     C                   clear                   v1rsr
026500011030     C                   clear                   v1rxs2
026600011030     C                   clear                   v1crc
026700011030     C                   clear                   v1rsc
026800011030     C                   clear                   v1rxs3
026900011023
027000011030     C                   ENDSR
027100011023
027200011024      **********************************************************************
027300011030      * F5 - Ricerca P.d.C.
027400011024      **********************************************************************
027500011024     C     Sr_RicPDC     BEGSR
027600011024
027700011024     C                   eval      *in90 = *on
027800011026
027900011030     C                   call      'XALFA3BR'
028000080722     C                   parm      Rsut          PAXDUT           30
028100011026     C                   parm      2             PAXKUT            1 0
028200011026     C                   parm      v1rags        PAXRAG           48
028300011026     C                   parm      kci           PAXCCC            4 0
028400011026     C                   parm      0             PAXSTA            1 0
028500011026     C                   parm      *blanks       PAXFLR           90
028600011026     C                   parm      *blanks       PAXDIT            3
028700011026     C                   parm      1             PAXNUM            2 0
028800011026     C                   parm      *blanks       PAXKCM           80
028900011026     C                   parm      *blanks       PAXKSM          140
029000011026     C                   parm      *blanks       PAXKDM           60
029100011026
029200011026      * codice selezionato
029300011026     C                   if        PAXsta >= 0
029400011026     C                   eval      v1idc  =  '0' + %subst(PAXksm:1:7)
029500011026     C                   eval      v1rags =  PAXrag
029600011112     C                   eval      *in45  =  *on
029700011026     C                   endif
029800011023
029900011030     C                   ENDSR
030000011030
030100011030      **********************************************************************
030200011030      * F7 - Ricerca Mittente
030300011030      **********************************************************************
030400011030     C     Sr_RicRitiro  BEGSR
030500011030
030600011030     C                   eval      *in90 = *on
030700131216     c                   clear                   FIOR81ds
030800131216     c                   IF        V1cra  > 0
030900131216     c                   eval      I81fil = V1cra1
031000131216     c                   ENDIF
031100131216     c                   IF        V1rsr <> *blanks
031200131216     c                   eval      I81rag = V1rsr
031300131216     c                   ENDIF
031400131216     c                   eval      I81mai = 'S'
031500131216     c                   call      'FIOR81R'
031600131216     c                   parm                    kpjba
031700131216     c                   parm                    FIOR81DS
031800131216
031900131216     c                   SELECT
032000131216     c                   WHEN      O81err <> *blanks
032100131216     c                   eval      V1Cmsg = O81msg
032200131216     c                   seton                                        48  28
032300131216     c                   leavesr
032400131216     c                   WHEN      O81ret = '1'
032500131216     c                   leavesr
032600131216     c                   WHEN      O81cro <> *zeros
032700131216     c                   movel     O81cro        V1cra
032800131216     c     O81cro        chain     FNACR000
032900070809if  2c                   if        %found(FNACR01L)
033000070809     c                   movel     ACRrsr        V1rsr
033100070809e   2c                   endif
033200070809     c                   eval      *in48  =  *on
033300131216     c                   ENDSL
033400011030
033500011030     C                   ENDSR
033600011030
033700011030      **********************************************************************
033800011030      * F9 - Ricerca Destinatario
033900011030      **********************************************************************
034000011030     C     Sr_RicDestin  BEGSR
034100011030
034200011030     C                   eval      *in90 = *on
034300131216     c                   clear                   FIOR81ds
034400131216     c                   IF        V1crc  > 0
034500131216     c                   eval      I81fil = V1crc1
034600131216     c                   ENDIF
034700131216     c                   IF        V1rsc <> *blanks
034800131216     c                   eval      I81rag = V1rsc
034900131216     c                   ENDIF
035000131216     c                   eval      I81mai = 'S'
035100131216     c                   call      'FIOR81R'
035200131216     c                   parm                    kpjba
035300131216     c                   parm                    FIOR81DS
035400131216
035500131216     c                   SELECT
035600131216     c                   WHEN      O81err <> *blanks
035700131216     c                   eval      V1Cmsg = O81msg
035800131216     c                   seton                                        50  28
035900131216     c                   leavesr
036000131216     c                   WHEN      O81ret = '1'
036100131216     c                   leavesr
036200131216     c                   WHEN      O81cro <> *zeros
036300131216     c                   movel     O81cro        V1crc
036400131216     c     O81cro        chain     FNACR000
036500070809if  2c                   if        %found(FNACR01L)
036600070809     c                   movel     ACRrsr        V1rsc
036700070809e   2c                   endif
036800070809     c                   eval      *in50  =  *on
036900131216     c                   ENDSL
037000011030
037100011030     C                   ENDSR
037200011024
037300011023      **********************************************************************
037400011023      * CONTROLLI VIDEO 1
037500011023      **********************************************************************
037600011023     C     Sr_Contr01    begsr
037700011023
037800011023      * Reset indicatori
037900011030     C                   setoff                                       28
038000011112     C                   setoff                                       404142
038100011112     C                   setoff                                       434445
038200011112     C                   setoff                                       464748
038300011112     C                   setoff                                       495051
038400011107
038500011107      * Pulizia campi descrittivi
038600011107     C                   clear                   v1dpoe
038700011112
038800011112      * L'identificativo cliente, se *zero è come se non immesso
038900011112IF  1C                   if        v1idc  =  *zeros
039000011112     C                   clear                   v1idc
039100011112E   1C                   endif
039200011023
039300011024      * deve essere effettuata almeno una selezione
039400011024IF  1C                   if            v1dida = 0
039500011024     C                             and v1oida = 0
039600011024     C                             and v1dia  = 0
039700011024     C                             and v1oia  = 0
039800011112     C                             and v1idc  = *blanks
039900011030     C                             and v1noid = *blanks
040000011024     C                             and v1rfa  = *blanks
040100011030     C                             and v1rxs1 = *blanks
040200011107     C                             and v1noco = *blanks
040300011107     C                             and v1cpoe = 0
040400011030     C                             and v1cra  = 0
040500011030     C                             and v1rsr  = *blanks
040600011030     C                             and v1rxs2 = *blanks
040700011030     C                             and v1crc  = 0
040800011030     C                             and v1rsc  = *blanks
040900011030     C                             and v1rxs3 = *blanks
041000011025     C                   seton                                        40  28
041100011024     C                   movel     msg(1)        v1cmsg
041200011030     C                   goto      EndSrContr
041300011024E   1C                   endif
041400011023
041500011024      * data immissione dal non valida
041600011024IF  1C                   if        v1dida <> 0
041700011024     C                   clear                   wlbdat
041800011024     C                   z-add     v1dida        G02dat
041900011024     C                   call      'XSRDA8'
042000011024     C                   parm                    wlbdat
042100011024IF  2C                   if        G02err = '1'
042200011025     C                   seton                                        41  28
042300011024     C                   movel     msg(2)        v1cmsg
042400011030     C                   goto      EndSrContr
042500011024X   2C                   else
042600011024     C                   z-add     G02dat        v1dida
042700011024     C                   z-add     G02inv        w1dida
042800011024E   2C                   endif
042900011024X   1C                   else
043000011024     C                   z-add     0             w1dida
043100011024E   1C                   endif
043200011031
043300011031      * data immissione al  in formato non valido
043400011031IF  1C                   if        v1dia  <> 0
043500011031     C                   clear                   wlbdat
043600011031     C                   z-add     v1dia         G02dat
043700011031     C                   call      'XSRDA8'
043800011031     C                   parm                    wlbdat
043900011031IF  2C                   if        G02err = '1'
044000011031     C                   seton                                        42  28
044100011031     C                   movel     msg(2)        v1cmsg
044200011031     C                   goto      EndSrContr
044300011031X   2C                   else
044400011031     C                   z-add     G02dat        v1dia
044500011031     C                   z-add     G02inv        w1dia
044600011031E   2C                   endif
044700011031X   1C                   else
044800011031     C                   z-add     *all'9'       w1dia
044900011031E   1C                   endif
045000011031
045100011031      * sequenza date non valida
045200011031IF  1C                   if        w1dida > w1dia
045300011031     C                   seton                                        414228
045400011031     C                   movel     msg(4)        v1cmsg
045500011031     C                   goto      EndSrContr
045600011031E   1C                   endif
045700011024
045800011024      * ora immissione dalle non valida
045900011024IF  1C                   if        v1oida <> 0
046000011029IF  2C                   if        v1dida =  0
046100011031     C                   seton                                        43  28
046200011029     C                   movel     msg(5)        v1cmsg
046300011030     C                   goto      EndSrContr
046400011029E   2C                   endif
046500011029     C     v1oida        mult      100           w1time
046600011031     C     *hms          test(t)                 w1time                 43
046700011024IF  2C                   if        *in42  =  *on
046800011025     C                   seton                                            28
046900011024     C                   movel     msg(3)        v1cmsg
047000011030     C                   goto      EndSrContr
047100011025X   2C                   else
047200011029     C     v1oida        mult      100           w1oida
047300011024E   2C                   endif
047400011024X   1C                   else
047500011025     C                   clear                   w1oida
047600011024E   1C                   endif
047700011023
047800011024      * ora immissione alle  non valida
047900011024IF  1C                   if        v1oia  <> 0
048000011029IF  2C                   if        v1dia  =  0
048100011029     C                   seton                                        44  28
048200011029     C                   movel     msg(5)        v1cmsg
048300011030     C                   goto      EndSrContr
048400011029E   2C                   endif
048500011029     C     v1oia         mult      100           w1time
048600011029     C     *hms          test(t)                 w1time                 44
048700011024IF  2C                   if        *in44  =  *on
048800011025     C                   seton                                            28
048900011024     C                   movel     msg(3)        v1cmsg
049000011030     C                   goto      EndSrContr
049100011025X   2C                   else
049200011029     C     v1oia         mult      100           w1oia
049300011031     C                   add       59            w1oia
049400011024E   2C                   endif
049500011024X   1C                   else
049600011025     C                   z-add     *all'9'       w1oia
049700011024E   1C                   endif
049800011026
049900011026      * sequenza orari non valida
050000011026     C                   if        w1oida > w1oia
050100011031     C                   seton                                        434428
050200011029     C                   movel     msg(6)        v1cmsg
050300011030     C                   goto      EndSrContr
050400011026E   1C                   endif
050500011024
050600011024      * identificativo cliente non valido
050700011026IF  1C                   if        v1idc  <> *blanks
050800011026      *   controllo numericità
050900011030     C     DigitN        check     v1idc                                  45
051000011026IF  2C                   if        *in45
051100011025     C                   seton                                            28
051200011029     C                   movel     msg(7)        v1cmsg
051300011030     C                   goto      EndSrContr
051400011026X   2C                   else
051500011026      *   controllo anagrafico
051600011026     C                   move      v1idc         ACOksc
051700011026     C     kcnaco        chain     CNACO00F
051800011026B   3C                   if        %found(CNACO00F)
051900011026     C                             and ACOflg = *blanks
052000011031IF  4C                   if        *in90 = *off  and
052100011031     C                             %subst(ACOrag:1:Wsize1) <> v1rags
052200011031     C                   seton                                        90
052300011031E   4C                   endif
052400011026     C                   movel     ACOrag        v1rags
052500011026X   3C                   else
052600011026     C                   clear                   v1rags
052700011026     C                   seton                                        45  28
052800011029     C                   movel     msg(8)        v1cmsg
052900011030     C                   goto      EndSrContr
053000011026E   3C                   endif
053100011026E   2C                   endif
053200011024E   1C                   endif
053300011025
053400011030      * flag "Solo O.R.M. senza ID cliente" in alternativa al codice cliente
053500011030IF  1C                   if            v1noid =  'S'
053600011025     C                             and v1idc  >  *zeros
053700011025     C                   seton                                        45  28
053800011029     C                   movel     msg(9)        v1cmsg
053900011030     C                   goto      EndSrContr
054000011025E   1C                   endif
054100011030
054200011030      * flag "ricerca per stringa" su Riferimento O.R.M. valido solo
054300011030      *  se immesso un codice (parziale) O.R.M.
054400011030IF  1C                   if            v1rxs1 =  'S'
054500011030     C                             and v1rfa  =  *blanks
054600011030     C                   seton                                        46  28
054700011030     C                   movel     msg(10)       v1cmsg
054800011030     C                   goto      EndSrContr
054900011030E   1C                   endif
055000011107
055100011107      * codice p.o. emittente non valido
055200011107IF  1C                   if        v1cpoe <> 0
055300011107     C     v1cpoe        chain     AZORG01L
055400011107IF  2C                   if        not %found(azorg01l)
055500011107     C                   seton                                        47  28
055600011107     C                   movel     msg(11)       v1cmsg
055700011107     C                   goto      EndSrContr
055800011107X   2C                   else
055900011107     C                   movel     ORGdes        v1dpoe
056000011107E   2C                   endif
056100011107E   1C                   endif
056200011030
056300011107      * codice cliente mittente non valido
056400011107IF  1C                   if        v1cra <> 0
056500011030     C     v1cra         chain     FNACR01L
056600011030IF  2C                   if        not %found(fnacr01l)
056700011107     C                   seton                                        48  28
056800011107     C                   movel     msg(12)       v1cmsg
056900011030     C                   goto      EndSrContr
057000011030X   2C                   else
057100011109IF  3C                   if        *in90 = *off  and
057200011109     C                             %subst(ACRrsr:1:Wsize2) <> v1rsr
057300011109     C                   seton                                        90
057400011109E   3C                   endif
057500011030     C                   movel     ACRrsr        v1rsr
057600011030E   2C                   endif
057700011030E   1C                   endif
057800011030
057900011030      * flag "ricerca per stringa" su Rag.Sociale Mitt. valido solo
058000011102      *  se immesso una Rag.Sociale (anche parziale) Mittente
058100011030IF  1C                   if            v1rxs2 =  'S'
058200011030     C                             and v1rsr  =  *blanks
058300011107     C                   seton                                        49  28
058400011107     C                   movel     msg(13)       v1cmsg
058500011030     C                   goto      EndSrContr
058600011030E   1C                   endif
058700011102
058800011102      * ...ma non valido se immesso il codice Mittente
058900011102IF  1C                   if            v1rxs2 =  'S'
059000011102     C                             and v1cra  <> 0
059100011107     C                   seton                                        484928
059200011107     C                   movel     msg(16)       v1cmsg
059300011102     C                   goto      EndSrContr
059400011102E   1C                   endif
059500011030
059600011030      * codice cliente destinatario non valido
059700011030IF  1C                   if        v1crc <> 0
059800011030     C     v1crc         chain     FNACR01L
059900011030IF  2C                   if        not %found(fnacr01l)
060000011107     C                   seton                                        50  28
060100011107     C                   movel     msg(14)       v1cmsg
060200011030     C                   goto      EndSrContr
060300011030X   2C                   else
060400011109IF  3C                   if        *in90 = *off  and
060500011109     C                             %subst(ACRrsr:1:Wsize3) <> v1rsc
060600011109     C                   seton                                        90
060700011109E   3C                   endif
060800011030     C                   movel     ACRrsr        v1rsc
060900011030E   2C                   endif
061000011030E   1C                   endif
061100011030
061200011030      * flag "ricerca per stringa" su Rag.Sociale Dest. valido solo
061300011102      *  se immesso una Rag.Sociale (anche parziale) Destinatario
061400011102IF  1C                   if            v1rxs3 =  'S'
061500011102     C                             and v1rsc  =  *blanks
061600011107     C                   seton                                        51  28
061700011107     C                   movel     msg(15)       v1cmsg
061800011030     C                   goto      EndSrContr
061900011030E   1C                   endif
062000011102
062100011102      * ...ma non valido se immesso il codice Destinatario
062200011102IF  1C                   if            v1rxs3 =  'S'
062300011102     C                             and v1crc  <> 0
062400011107     C                   seton                                        505128
062500011107     C                   movel     msg(16)       v1cmsg
062600011102     C                   goto      EndSrContr
062700011102E   1C                   endif
062800011024
062900011030     C     EndSrContr    ENDSR
063000011024
063100011024      *****************************************************************
063200011024      * IMPOSTO OPZIONI
063300011024      *****************************************************************
063400011024     C     Sr_Caropz     BEGSR
063500011024
063600011024     C                   movel     dopz1         v2dse1
063700011024
063800011024     C                   ENDSR
063900011024
064000011023      **********************************************************************
064100011023      * GESTIONE SUBFILE
064200011023      **********************************************************************
064300011023     C     Sr_Gessfl     begsr
064400011023
064500011024     C* inizializzazione videata
064600011024     C                   exsr      Sr_Inzsfl
064700011024
064800011024DO  1C                   do        *hival
064900011023
065000011023      * Se non ho caricato emetto una videata di segnalazione.
065100011024IF  2C                   if        nrr1 = *zeros
065200011031     C                   seton                                        91
065300011023     C                   exfmt     fior283
065400011031X   2C                   else
065500011023      * Se ho caricato emetto il subfile
065600011031     C                   setoff                                       91
065700011024     C                   write     fior282
065800011024     C                   exfmt     fior28c
065900011024E   2C                   endif
066000011025
066100011025     C                   setoff                                       2890
066200011030     C                   clear                   v2cmsg
066300011023
066400011023      * F3=Fine
066500011023     C   kc              leave
066600011023
066700011023      * F12=Ritorno
066800011023     C   kl              leave
066900011025
067000011025      * Roll Up
067100011102IF  2C                   if        Tasto = X'F5'
067200011102     C                   exsr      Sr_Pagina
067300011102     C                   iter
067400011102E   2C                   endif
067500011023
067600011024      *    Leggo Scelte Effettuate dall'utente
067700011023
067800011024IF  2C                   if        SFLNRR > 0
067900011024     C                   z-add     SFLNRR        RecSF
068000011024E   2C                   endif
068100011024
068200011025     C                   exsr      Sr_Contr02
068300011023
068400011024E   1C                   enddo
068500011023
068600011023     C                   endsr
068700011024
068800011024      **********************************************************************
068900011025      * CONTROLLO OPZIONI SUBFILE (VIDEO 2)
069000011024      **********************************************************************
069100011025     C     Sr_Contr02    begsr
069200011024
069300011024      * Leggo il sfl solo se ci sono rcd
069400011024IF  1C                   if        wMax > 0
069500011024
069600011024     C                   readc     fior28s                                10
069700011024
069800011024      * esco se fine sfl
069900011024DO  2C                   dow       *in10 = *off
070000011024
070100011025      * riposiziono il cursore sull'ultimo record modificato
070200011024     C                   z-add     nrr1          RecSF
070300011025
070400011025      * reset indicatori
070500011106     C                   setoff                                       2829
070600011030     C                   setoff                                       80
070700011025
070800011024      * gestione opzioni
070900011024CAS 3C     v2scel        casne     *blanks       Sr_Opzioni
071000011024E   3C                   endcs
071100011024      * se rilevato errore o richiesta uscita, attivo sflnxtchg
071200011025     C   28
071300011025     Cor 90              eval      *in32 = *on
071400011025     C  n28              clear                   v2scel
071500011024
071600011024     C                   update    fior28s
071700011025     C                   eval      *in32 = *off
071800011024
071900011025     C* leggo il prossimo rcd dal sfl se non rilevati errori
072000011106     C*  e se non è stato premuto F3 dal pgm. chiamato
072100011106     C   28
072200011106     Cor 29              leave
072300011024     C                   readc     fior28s                                10
072400011025     C   10              z-add     SflPag        wPag
072500011024
072600011024E   2C                   enddo
072700011024
072800011024E   1C                   endif
072900011024
073000011024     C                   endsr
073100011024
073200011024      **********************************************************************
073300011024      * GESTIONE OPZIONI SUBFILE
073400011024      **********************************************************************
073500011024     C     Sr_Opzioni    begsr
073600011024
073700011024SEL 1C                   select
073800011024
073900011030      * Visualizzazione VAS
074000011024W   1C                   when      v2scel = '5'
074100011025
074200011030     C                   clear                   fior29ds
074300011030     C                   z-add     h2prg         or29prg
074400011024
074500011030     C                   movel     fior29ds      kpjbu
074600011031     C                   call      'FIOR29R'
074700011024     C                   parm                    kpjba
074800011030     C                   movel     kpjbu         fior29ds
074900011106
075000011106IF  2C                   if        or29end = *on
075100011106     C                   movel     *on           *in29
075200011106E   2C                   endif
075300011030
075400011030      * Visualizzazione ORM
075500011030W   1C                   when      v2scel = '9'
075600011030
075700011107IF  2C                   if            v2nsr = 0
075800011030     C                             and v2nor = 0
075900011030     C                             and v2nrv = 0
076000011030
076100011030     C                   seton                                        80  28
076200011107     C                   movel     msg(17)       v2cmsg
076300011030     C                   goto      endsropzioni
076400011030
076500011106X   2C                   else
076600011030
076700011030     C                   clear                   fior07ds
076800011030IF  3C                   if        v2por = v2poe
076900011030     C     v2por         lookup    l6                                     10
077000011030IF  4C                   if        *in10 = *on
077100011030     C                   eval      pmio = 'S'
077200011030E   4C                   endif
077300011030E   3C                   endif
077400011109     C                   z-add     h2poe         ppoe
077500011107     C                   z-add     v2nor         pnor
077600011107     C                   z-add     v2nsr         pnsr
077700011107     C                   z-add     v2nrv         pnrv
077800011030     C                   movel     v2scel        psce
077900011030
078000011030     C                   movel     fior07ds      kpjbu
078100011030     C                   call      'FIOR07R'
078200011030     C                   parm                    kpjba
078300011030     C                   movel     kpjbu         fior07ds
078400011030
078500011030E   2C                   endif
078600011024
078700011024E   1C                   endsl
078800011024
078900011025     C     endsropzioni  endsr
079000011024
079100011024      **********************************************************************
079200011024      * INIZIALIZZAZIONE SUBFILE
079300011024      **********************************************************************
079400011024     C     Sr_Inzsfl     begsr
079500011024
079600011024     C* pulizia SFL
079700011024     C                   eval      *in30 = *off
079800011024     C                   eval      *in31 = *off
079900011024     C                   write     fior28c
080000011024     C                   eval      *in31 = *on
080100011024
080200011024     C* caricamento SFL a pagine
080300011024     C                   clear                   nrr1
080400011024     C                   clear                   WMAX
080500011024
080600011024     C* Posizionamento iniziale
080700011024     C                   exsr      Sr_Setll
080800011024     C* Lettura file
080900011024     C                   exsr      Sr_Read
081000011024
081100011024     C* carico solo la prima pagina di dati
081200011025     C                   z-add     SflPag        wPag
081300011024     C*
081400011024     C                   exsr      Sr_Pagina
081500011024     C* imposto il caricamento sfl del numero massimo di righe per pagina
081600011025     C                   z-add     SflPag        wPag
081700011024
081800011024     C                   endsr
081900011024
082000011024      **********************************************************************
082100011024      * POSIZIONAMENTO SUI FILES PILOTA
082200011024      **********************************************************************
082300011024     C     Sr_Setll      begsr
082400011024
082500011024SEL 1C                   select
082600011024      *
082700011026W   1C                   when      v1idc  <> *blanks
082800011024     C                   eval      wORD = 2
082900011024     C     v1idc         setll     tiori02l
083000011024      *
083100011024W   1C                   when      V1rfa  <> *blanks
083200011024     C                   eval      wORD = 3
083300011030IF  2C                   if        v1rxs1 =  *blanks
083400011030     C     v1rfa         setll     tiori03l
083500011030X   2C                   else
083600011030     C     *loval        setll     tiori03l
083700011030E   2C                   endif
083800011024      *
083900011024W   1C                   when          v1dida <> 0
084000011024     C                             or  v1oida <> 0
084100011024     C                             or  v1dia  <> 0
084200011024     C                             or  v1oia  <> 0
084300011024     C                   eval      wORD = 4
084400011024     C     k2tiori4      setll     tiori04l
084500011024      *
084600011024X   1C                   other
084700011024     C                   eval      wORD = 1
084800011024     C     *loval        setll     tiori01l
084900011024      *
085000011024E   1C                   endsl
085100011024
085200011024     C                   endsr
085300011024
085400011024      **********************************************************************
085500011024      * LETTURA RCD ARCHIVIO PILOTA
085600011024      **********************************************************************
085700011024     C     Sr_Read       begsr
085800011024
085900011025     C                   eval      Ok_rec = *off
086000011025     C                   eval      *in33  = *off
086100011024
086200011025DO  1C                   DOU       *in33
086300011024     C                             or Ok_rec = *on
086400011024
086500011024SEL 2C                   select
086600011024      *
086700011024W   2C                   when      wOrd = 2
086800011025     C     v1idc         reade     tiori02l                               33
086900011024      *
087000011024W   2C                   when      wOrd = 3
087100011105IF  3C                   if        v1rxs1 =  *blanks
087200011105     C     v1rfa         reade     tiori03l                               33
087300011105X   3C                   else
087400011030     C                   read      tiori03l                               33
087500011105E   3C                   endif
087600011024      *
087700011024W   2C                   when      wOrd = 4
087800011025     C                   read      tiori04l                               33
087900011024      *
088000011024X   2C                   other
088100011025     C                   read      tiori01l                               33
088200011024      *
088300011024E   2C                   endsl
088400011024
088500011025IF  2C                   if        not *in33
088600011024     C                   exsr      Sr_Selez
088700011024E   2C                   endif
088800011024
088900011024E   1C                   enddo
089000011024
089100011024     C                   endsr
089200011024
089300011024      ************************************************************
089400011024      * SELEZIONI SULLA RIGA LETTA
089500011024      ************************************************************
089600011024
089700011024     C     Sr_Selez      begsr
089800011024
089900011024     C                   movel     *ON           Ok_rec
090000011025
090100011026      * selezione record
090200011025
090300011024SEL 1C                   select
090400011029      *
090500011026      *   record annullato
090600080722W   1C**!!!VAOatb        wheneq    'A'
090700080722     C**!!!              movel     *off          Ok_rec
090800011025      *
090900011025      *   data elaborazione non compresa nella scelta
091000011024W   1C     ORIder        whenlt    w1dida
091100011024     C     ORIder        orgt      w1dia
091200011024     C                   movel     *off          Ok_rec
091300011102IF  2C                   if        wORD = 4
091400011102     C                             and ORIder > w1dia
091500011102     C                   movel     *on           *in33
091600011102E   2C                   endif
091700011025      *
091800011025      *   ora elaborazione non compresa nella scelta
091900011025W   1C     ORIoer        whenlt    w1oida
092000011025     C     ORIoer        orgt      w1oia
092100011024     C                   movel     *off          Ok_rec
092200011025      *
092300011025      *   identificativo cliente non compresa nella scelta
092400011024W   1C     v1idc         whenne    *blanks
092500011024     C     ORIidc        andne     v1idc
092600011024     C                   movel     *off          Ok_rec
092700011102IF  2C                   if        wORD = 2
092800011102     C                   movel     *on           *in33
092900011102E   2C                   endif
093000011025      *
093100011025      *   cliente codificato non compreso nella scelta
093200011102W   1C     v1noid        wheneq    'S'
093300011025     C     ORIidc        andne     *blanks
093400011102     C     ORIidc        andne     *zeros
093500011025     C                   movel     *off          Ok_rec
093600011025      *
093700011025      *   riferimento ORM non compreso nella scelta
093800011024W   1C     v1rfa         whenne    *blanks
093900011107     C     v1rxs1        andeq     *blanks
094000011107     C     v1rfa         andne     VAOrfa
094100011030     C                   movel     *off          Ok_rec
094200011107IF  2C                   if        wORD = 3
094300011030     C                   movel     *on           *in33
094400011107E   2C                   endif
094500011107      *
094600011107      *   ORM confermato non compreso nella scelta
094700011107W   1C     v1noco        wheneq    'S'
094800011107     C     ORInor        andne     0
094900011107     C                   movel     *off          Ok_rec
095000011107      *
095100011107      *   codice P.O. emittente non compreso nella scelta
095200011107W   1C     v1cpoe        whenne    0
095300011107     C     VAOpoe        andne     v1cpoe
095400011107     C                   movel     *off          Ok_rec
095500011030      *
095600011030      *   codice cliente mittente non compreso nella scelta
095700011030W   1C     v1cra         whenne    0
095800011030     C     VAOcra        andne     v1cra
095900011030     C                   movel     *off          Ok_rec
096000011030      *
096100011030      *   codice cliente mittente non compreso nella scelta
096200011030W   1C     v1crc         whenne    0
096300011030     C     VAOcrc        andne     v1crc
096400011030     C                   movel     *off          Ok_rec
096500011025      *
096600011024E   1C                   endsl
096700011029
096800011029     C     Ok_Rec        cabeq     *off          EndSelez
096900011029
097000011029      * ulteriori selezione record
097100011029
097200011107      *   riferimento ORM non compreso nella scelta (ricerca parziale)
097300011107W   1C                   if            v1rfa <> *blanks
097400011107     C                             and v1rxs1 = 'S'
097500011107     C                   movel(p)  v1rfa         wTest
097600011107     C     ' '           checkr    wTest         xx
097700011107     C     v1rfa:xx      scan      VAOrfa                                 10
097800011107IF  3C                   if        not *in10
097900011107     C                   movel     *off          Ok_rec
098000011109     C                   goto      EndSelez
098100011107E   3C                   endif
098200011107E   2C                   endif
098300011107      *
098400011029      *   ragione sociale mittente non compreso nella scelta
098500011030IF  1C                   if            v1cra  = 0
098600011030     C                             and v1rsr <> *blanks
098700011030     C                   movel(p)  v1rsr         wTest
098800011030     C     ' '           checkr    wTest         xx
098900011030IF  2C                   if        v1rxs2     = *blanks
099000011030IF  3C                   if        %subst(v1rsr:1:xx) <> %subst(VAOrsr:1:xx)
099100011030     C                   movel     *off          Ok_rec
099200011030E   3C                   endif
099300011030X   2C                   else
099400011030     C     v1rsr:xx      scan      VAOrsr                                 10
099500011029IF  3C                   if        not *in10
099600011029     C                   movel     *off          Ok_rec
099700011109     C                   goto      EndSelez
099800011029E   3C                   endif
099900011029E   2C                   endif
100000011029E   1C                   endif
100100011029      *
100200011029      *   ragione sociale destinatario non compreso nella scelta
100300011030IF  1C                   if            v1crc  = 0
100400011030     C                             and v1rsc <> *blanks
100500011030     C                   movel(p)  v1rsc         wTest
100600011030     C     ' '           checkr    wTest         xx
100700011030B   2C                   if        v1rxs3     = *blanks
100800011030IF  3C                   if        %subst(v1rsc:1:xx) <> %subst(VAOrsc:1:xx)
100900011030     C                   movel     *off          Ok_rec
101000011109     C                   goto      EndSelez
101100011030E   3C                   endif
101200011029X   2C                   else
101300011030     C     v1rsc:xx      scan      VAOrsc                                 10
101400011029IF  3C                   if        not *in10
101500011029     C                   movel     *off          Ok_rec
101600011109     C                   goto      EndSelez
101700011029E   3C                   endif
101800011029E   2C                   endif
101900011029E   1C                   endif
102000011024
102100011029     C     EndSelez      endsr
102200011024
102300011024      ************************************************************
102400011025      * CARICAMENTO PAGINA LISTA
102500011024      ************************************************************
102600011024
102700011024     C     Sr_Pagina     begsr
102800011024
102900011024     C                   movel     *off          *in32
103000011024     C                   z-add     0             X
103100011024     C                   z-add     wMax          nrr1
103200011024      *
103300011024      * Leggo dal file per caricare la lista
103400011024      *
103500011025     C                   dow       not *in33
103600011024     C                             and X < wPag
103700011024      *
103800011024     C                   exsr      Sr_Carsfl
103900011024      *
104000011024     C                   exsr      Sr_Read
104100011024      *
104200011024     C                   enddo
104300011024      *
104400011024     C                   z-add     nrr1          wMax                 30
104500011024      *
104600011024      * Mi posiziono al 1° record della pagina
104700011024      *
104800011025     C     nrr1          div       SflPag        pagine
104900011025     C                   mvr                     resto
105000011025     C     pagine        mult      SflPag        RecSF
105100011025     C     resto         ifgt      0
105200011025     C                   add       1             RecSF
105300011025     C                   else
105400011025     C                   sub       SflPag        RecSF
105500011025     C                   add       1             RecSF
105600011025     C                   endif
105700011024
105800011024     C                   endsr
105900011024
106000011023      **********************************************************************
106100011023      * CARICO SUBFILE
106200011023      **********************************************************************
106300011023     C     Sr_Carsfl     begsr
106400011025
106500011025      * reperimento dati anagrafici mancanti all'anagrafica clienti ritiro
106600011025      *  - per cliente mittente:
106700011025IF  1C                   if            VAOrsr =  *blanks
106800011025     C                             or  VAOlor =  *blanks
106900011025     C                             or (VAOnar =  *blanks and
107000011025     C                                 VAOprr =  *blanks)
107100011025     C     VAOcra        chain     FNACR01L
107200011025IF  2C                   if        %found(fnacr01l)
107300011025IF  3C                   if        VAOrsr =  *blanks
107400011025     C                   movel     ACRrsr        VAOrsr
107500011025E   3C                   endif
107600011025IF  3C                   if        VAOlor =  *blanks
107700011025     C                   movel     ACRlor        VAOlor
107800011025E   3C                   endif
107900011025IF  3C                   if        VAOnar =  *blanks
108000011025     C                   movel     ACRnar        VAOnar
108100011025E   3C                   endif
108200011025IF  3C                   if        VAOprr =  *blanks
108300011025     C                   movel     ACRprr        VAOprr
108400011025E   3C                   endif
108500011025E   2C                   endif
108600011025E   1C                   endif
108700011025      *  - per cliente destinatario:
108800011025IF  1C                   if        VAOrsc =  *blanks
108900011025     C     VAOcrc        chain     FNACR01L
109000011025IF  2C                   if        %found(fnacr01l)
109100011025     C                   movel     ACRrsr        VAOrsc
109200011025E   2C                   endif
109300011025E   1C                   endif
109400011025
109500011025     C                   clear                   fior28s
109600011023
109700011025      * campi hidden
109800011030     C                   eval      h2prg = ORIprg
109900011025     C                   eval      h2poe = ORIpoe
110000011109     C                   eval      v2poe = VAOpoe
110100011109     C                   eval      v2por = VAOpor
110200011025
110300080722      * flag annullato
110400080722      * controllo se = 'A' perchè capita che nel campo ci siano delle
110500080722      * schifezze
110600080722     c                   if        vaoatb = 'A'
110700080722     c                   eval      v2atb  = vaoatb
110800080722     c                   else
110900080722     c                   clear                   v2atb
111000080722     c                   endif
111100011024      * riferimento ORM
111200011024     C                   eval      v2rfa  = VAOrfa
111300011024      * identificativo cliente
111400011102IF  1C                   if        ORIidc <> *zeros
111500011024     C                   eval      v2idc  = ORIidc
111600011102E   1C                   endif
111700011024      * data ritiro
111800011024     C                   z-add     ORIder        G02inv
111900011024     C                   movel     '3'           G02err
112000011024     C                   call      'XSRDA8'
112100011024     C                   parm                    wlbdat
112200011024     C                   movel     G02dat        v2der
112300011029      * ora ritiro (solo HH:MM, no SS)
112400011029     C                   eval      v2oer  = ORIoer / 100
112500011107      * P.O. emittente
112600011109      * (alfanumerico a video per visualizzare BLANK se "000",
112700011109      *  ma visualizzare gli zeri non significativi se > 000)
112800011109      *
112900011109IF  1C                   if        VAOpoe > 0
113000011109     C                   move      VAOpoe        v2poea
113100011109E   1C                   endif
113200011024      * serie effettiva
113300011024     C                   eval      v2nsr  = ORInsr
113400011024      * numero ORM effettivo
113500011024     C                   eval      v2nor  = ORInor
113600011024      * viaggio effettivo
113700011024     C                   eval      v2nrv  = ORInrv
113800140708      /free
113900140708       //?Se ORM senza serie ma con stesso numero ORM sui campi ORI e VAO
114000140708       //?vuol dire che è un ORM ancora da confermare quindi NON visualizzo il numero
114100140708         IF  ORInsr = 0 and ORInor > 0 and
114200140708                            ORIpoe = VAOpoe and ORInsr = VAOnsr and
114300140708                            ORInor = VAOnor and ORInrv = VAOnrv;
114400140708           clear v2nor;
114500140708           clear v2nrv;
114600140708         ENDIF;
114700140708      /end-free
114800011024      * P.O. ritiro
114900011109      * (alfanumerico a video per visualizzare BLANK se "000",
115000011109      *  ma visualizzare gli zeri non significativi se > 000)
115100011109IF  1C                   if        VAOpor > 0
115200011109     C                   move      VAOpor        v2pora
115300011109E   1C                   endif
115400011024      *
115500011025      * RITIRO :
115600011025      *   Ragione sociale Mittente
115700011024     C                   movel     VAOrsr        v2rsr
115800011025      *   Località ritiro
115900011024     C                   movel     VAOlor        v2lor
116000011025      *   Nazione / Provincia ritiro
116100011025     C                   if        VAOnar <> *blanks
116200011025     C                   movel     VAOnar        v2nar
116300011025     C                   else
116400011025     C                   movel(p)  VAOprr        v2nar
116500011025     C                   endif
116600011025      *
116700011025      * CONSEGNA:
116800011025      *   Ragione sociale Destinatario
116900011024     C                   movel     VAOrsc        v2rsc
117000011023
117100011024     C                   add       1             nrr1
117200011024     C                   add       1             X
117300011024
117400011024     C                   write     fior28s
117500011023
117600011023     C                   endsr
117700011024
117800011023      *****************************************************************
117900011023      * ROUTINE INIZIALE
118000011023      *****************************************************************
118100011023     C     *INZSR        BEGSR
118200011024
118300011023     C     *ENTRY        PLIST
118400011023     C                   PARM                    KPJBA
118500011024
118600011023     C                   movel     TIT_A         VTCtit
118700011109      * reperisco le lunghezze dei campi "descrittivi"
118800011031     C                   EVAL      wSize1 = %size(v1rags)
118900011109     C                   EVAL      wSize2 = %size(v1rsr)
119000011109     C                   EVAL      wSize3 = %size(v1rsc)
119100011024
119200080722     c     *dtaara       define    §azute        azuteds
119300080722     c     *dtaara       define    §datiute      ddatiute
119400080722      *
119500080722     c                   in(E)     *dtaara
119600080722     c                   IF        %ERROR or RSUT = *blanks
119700080722     c                   clear                   Tibs34Ds
119800080722     c                   call      'TIBS34R'
119900080722     c                   parm                    Tibs34Ds
120000080722     c                   in        *dtaara
120100080722     c                   ENDIF
120200120523
120300120523      /free
120400120523       //?Imposto libreria/file per file TIORI
120500120523        IF  %subst(knsif : 7 : 1) = 'P';
120600120523          wLib = 'GAITRAGRPS/';
120700120523          %subst(wLib:12:8) = 'TIORI01L';
120800120523          open TIORI01L;
120900120523          %subst(wLib:12:8) = 'TIORI02L';
121000120523          open TIORI02L;
121100120523          %subst(wLib:12:8) = 'TIORI03L';
121200120523          open TIORI03L;
121300120523          %subst(wLib:12:8) = 'TIORI04L';
121400120523          open TIORI04L;
121500120523        ENDIF;
121600120523        IF  %subst(knsif : 7 : 1) <> 'P';
121700120523          wLib = 'GAITRAGRU/';
121800120523          %subst(wLib:11:8) = 'TIORI01L';
121900120523          open TIORI01L;
122000120523          %subst(wLib:11:8) = 'TIORI02L';
122100120523          open TIORI02L;
122200120523          %subst(wLib:11:8) = 'TIORI03L';
122300120523          open TIORI03L;
122400120523          %subst(wLib:11:8) = 'TIORI04L';
122500120523          open TIORI04L;
122600120523        ENDIF;
122700120523      /end-free
122800011024
122900011023      * Da sede non imposto il p.o. di gestione
123000011023     C                   if        simfel = 0
123100011023     C                   eval      *in12 = *on
123200011023     C                   else
123300011024     C                   eval      *in12 = *off
123400011024      * imposto il p.o. di gestione
123500080722     C                   if        dutlpo = '2' or dutlpo = *blanks
123600080722     C                   movel     dutpou        wFGS
123700011023     C                   eval      *in06 = *on
123800011023     C                   else
123900011024     C                   movel     SIMFEL        wFGS
124000011023     C                   endif
124100011023     C                   endif
124200011024      * Carico £6
124300011024     C                   CLEAR                   trul06ds
124400011024     C                   MOVE      '£6'          D06COD
124500011024     C                   MOVEL     wFGS          D06KEY
124600011024     C                   MOVEL     trul06ds      KPJBU
124700011024     C                   CALL      'TRUL06R'
124800011109     C                   PARM                    KPJBA
124900011024     C                   MOVEL     KPJBU         trul06ds
125000011024     C                   MOVEA     LIN           L6
125100011024
125200011024     C     Kcnaco        klist
125300011024     C                   kfld                    codut
125400011024     C                   kfld                    kci
125500011024     C                   kfld                    ACOksc
125600011024
125700011024     C     k2tiori4      klist
125800011024     C                   kfld                    w1dida
125900011024     C                   kfld                    w1oida
126000011024
126100011024     C                   ENDSR
126200011024
126300011024      *------------------------------------------------------------------------*
126400011024
126500011023** MSG  Lungh. 78                                                            *
126600011024Effettuare almeno una selezione                                                     1
126700011024Data errata                                                                         2
126800011029Orario errato (deve essere in formato "ORE : MINUTI")                               3
126900011029"Data immissione dal" non può essere maggiore di "Data immissione al"               4
127000011029Ora non impostabile senza data                                                      5
127100011029"Ora immissione dalle" non può essere maggiore di "Ora immissione alle"             6
127200011029Codice cliente errato: deve essere numerico di massimo 7 cifre                      7
127300011029Codice cliente Piano dei Conti errato                                               8
127400011030Per selezionare "Solo O.R.M. senza ID cliente" non immettere il codice cliente      9
127500011030Riferimento O.R.M. obbligatorio per la ricerca                                     10
127600070418Codice Filiale errata                                                              11
127700011107Codice cliente mittente errato                                                     12
127800011107Ragione sociale del cliente mittente obbligatoria per la ricerca                   13
127900011107Codice cliente destinatario errato                                                 14
128000011107Ragione sociale del cliente destinatario obbligatoria per la ricerca               15
128100011107"Ricerca per stinga" valida solo se non immesso il codice cliente                  16
128200011107O.R.M. non confermato                                                              17
