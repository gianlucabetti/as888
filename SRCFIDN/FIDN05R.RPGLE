000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200980601     F* fidn05r * -----------------------------------------*
000300980601     F* Ricerca / controllo / Ricerca fase successiva
000400000000     F*----------------------------------------------------*
000500980601     Ffndfa01l  iF   E           K DISK
000600980717     Ffndfd01l  iF   E           K DISK
000700981125     Ffndcf01l  iF   E           K DISK
000800990208     Ffidn05d   CF   E             WORKSTN usropn
000900980601     F                                     SFILE(dn05s01:nrr2)
001000980605     D MSG             S             78    DIM(05) CTDATA PERRCD(1)
001100000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001200000000     D KPJBA         E DS
001300980601     D fidn05        E DS                  extname(fidn05ds)
001400980601     D fndfa         E DS                  extname(fndfa00f)
001500971016     DWDATA            S               D   DATFMT(*EUR)
001600980601     D Kfca            S                   LIKE(dfafca)
001700980717     D tad             S                   LIKE(dfdtad)
001800981125     D kaac            S                   LIKE(dcfaac)
001900981125     D kfil            S                   LIKE(dcffil)
002000981125     D knca            S                   LIKE(dcfnca)
002100980717     D Wdataeur        S               D   DATFMT(*eur)
002200980717     D Wdataoggi       S               D   DATFMT(*eur)
002300980720     D w0140           s             14  0
002400980720     D dateu           s              8  0
002500980720     D wdtgio          s              8  0
002600981126     D okfase          s              1
002700980717     D                SDS
002800980717     D  VTCPGM                 1     10
002900980717     D WLBDAT          DS                  INZ
003000980717     D  G02DAT                 1      8  0
003100980717     D  G02INV                 9     16  0
003200980717     D  G02ERR                17     17
003300980717     D  G02TGI                18     22  0
003400971021     F*----------------------------------------------------*
003500971021      * INDICATORI
003600971021     c* 28   - emetto campo messaggi
003700981125     c* 30/32- comodo
003800971021     c* 70   - sfl control
003900971021     c* 71   - sfldsp
004000000000     C*----------------------------------------------------*
004100971016     C*
004200980601?    C* Controllo se e' stata richiesto CONTROLLO O RICERCA Fasi o Ricerca fase
004300980522     c* CONTROLLO
004400980601     c                   if        i05mod='C'
004500980522     c                   exsr      SUB_CONTR
004600980601     C                   endif
004700980601     c* ricerca fase successiva
004800980601     c                   if        i05mod='F'
004900980601     c                   exsr      SUB_SUCCE
005000980601     C                   endif
005100980522     c* RICERCA
005200980601     c                   if        i05mod='R'
005300980522     c                   exsr      SUB_RICER
005400980525     c* gestione
005500980525     c                   exsr      SUB_gesbf
005600980601     c* se il campo della ds O05REC e' pulito vuol dire che non e' stato selezio
005700980601     c                   if        o05rec = *blanks and o05err = ' '
005800980601     c                   move      'E'           o05err
005900980601     c                   move      msg(4)        o05msg
006000980526     c                   endif
006100990208     c*
006200990208     c                   close     fidn05d
006300990208     c*
006400980526     c                   endif
006500980522     c*
006600980526     C                   eval      *inlr = *on
006700980522     C*-------------------------------------------------------------------------
006800980601     c* SUB_CONTR Routine di Controllo Fase passata
006900980522     C*-------------------------------------------------------------------------
007000980522     c     SUB_CONTR     BEGSR
007100971015     C*
007200980601     c     i05fca        chain     fndfa01l                           30
007300980601     c* se record non trovato oppure presente ma annullato  errore
007400980601     c* annullato
007500980601     c                   if        *in30=*off and dfaatb <> *blank
007600980601     c                   move      'A'           O05err
007700980601     c                   move      msg(2)        O05msg
007800980522     c                   endif
007900980601     c                   if        *in30=*on
008000980601     c                   move      'E'           O05err
008100980601     c                   move      msg(1)        O05msg
008200980601     c                   endif
008300980522     c* se non c'è errore passo il record
008400980601     c                   if        o05err <> 'E'
008500980602     c                   movel     fndfa         O05rec
008600980522     c                   endif
008700980522     c*
008800980522     c                   endsr
008900980522     C*-------------------------------------------------------------------------
009000980522     c* SUB_RICER Routine di Ricerca
009100980522     C*-------------------------------------------------------------------------
009200980525     c     SUB_RICER     BEGSR
009300980522     C*
009400980526     c*
009500980522     C                   Z-ADD     0             nrr2              4 0
009600971021     C                   Z-ADD     1             CURS2
009700980721     C*
009800980525     C*-------- CARICA SUBFILE ----------------------------*
009900980601     C*
010000980601     C     *loval        SETLL     fndfa01l
010100980601     C                   READ      fndfa01l                               30
010200000000     C*
010300971021     C     *in30         DOweq     *off
010400980601     c                   if        dfaatb = ' '
010500971021     C                   ADD       1             nrr2
010600980601     c                   clear                   v5copz
010700980601     C                   MOVE      dfafca        v5cfca
010800980601     C                   MOVEl     dfades        v5cdes
010900980601     c* se fase obbligatoria
011000980601     c                   if        dfaobl=' '
011100980601     c                   move      'NO'          v5cobl
011200980601     c                   else
011300980601     c                   move      'SI'          v5cobl
011400980601     c                   endif
011500980601     c* se fase valida in liq.transatt. o in pratica assic. o sempre
011600980601     c                   if        dfaval=' '
011700980601     c                   move      'SI'          v5cvlt
011800980601     c                   move      'SI'          v5cvpa
011900980601     c                   endif
012000980601     c                   if        dfaval='T'
012100980601     c                   move      'SI'          v5cvlt
012200980601     c                   move      'NO'          v5cvpa
012300980601     c                   endif
012400980601     c                   if        dfaval='P'
012500980601     c                   move      'NO'          v5cvlt
012600980601     c                   move      'SI'          v5cvpa
012700980601     c                   endif
012800980601     c* se fase valida solo per anomalie di tipo evento oppure no
012900980723     c                   if        dfaene='E'
013000980604     c                   eval      v5cene='EVENTI   '
013100980601     c                   endif
013200980601     c                   if        dfaene=' '
013300980604     c                   eval      v5cene='SEMPRE   '
013400980601     c                   endif
013500980601     c                   if        dfaene='N'
013600980604     c                   eval      v5cene='NO EVENTI'
013700980601     c                   endif
013800981030     c* se fase valida solo per denuncie  no
013900981030     c                   if        dfafde='D'
014000981030     c                   eval      v5cden='DENUNCIA   '
014100981030     c                   endif
014200981030     c                   if        dfafde=' '
014300981030     c                   eval      v5cden='SEMPRE     '
014400981030     c                   endif
014500981030     c                   if        dfafde='N'
014600981030     c                   eval      v5cden='NO DENUNCIA'
014700981030     c                   endif
014800980601     c* cerco il p.o. abilitato alla fase
014900980601     c                   if        dfagfs='P'
015000980601     c                   move      'PARTENZA'    v5dgfs
015100980601     c                   endif
015200980601     c                   if        dfagfs='A'
015300980601     c                   move      ' ARRIVO '    v5dgfs
015400980601     c                   endif
015500980601     c                   if        dfagfs='S'
015600980601     c                   move      '  SEDE  '    v5dgfs
015700980601     c                   endif
015800981023     c                   if        dfagfs='F'
015900981026     c                   move      'PO APERT'    v5dgfs
016000981023     c                   endif
016100980601     c                   if        dfagfs=' '
016200980601     c                   move      ' TUTTI  '    v5dgfs
016300980601     c                   endif
016400980601     C                   WRITE     dn05s01
016500971021     c                   endif
016600971015     c**
016700980601     C                   READ      fndfa01l                               30
016800971015     C                   ENDdo
016900980525     c*
017000980525     c                   endsr
017100980525     C*-------------------------------------------------------------------------
017200980525     c* SUB_GESBF Routine Gestione Subfile
017300980525     C*-------------------------------------------------------------------------
017400980525     c     SUB_GESBF     BEGSR
017500980525     C*
017600971015     C* nessun record caricato
017700980526    1c                   if        nrr2 = 0
017800980525     c                   eval      *in71 = *off
017900980526     c* errore
018000980601     c                   move      'E'           O05err
018100980601     c                   move      msg(3)        O05msg
018200980526     c
018300980526    xc                   else
018400980525     c                   eval      *in71 = *on
018500980526    1c                   endif
018600000000     C*
018700881025?    C     T1TAG         TAG
018800980601     C                   WRITE     dn05z01
018900980601     C                   WRITE     dn05t01
019000980526?   1C                   dow       *inkl = *off
019100971015     c* sfl controlo
019200980601     C                   EXFMT     dn05c01
019300980601     c* F12= fine lavoro
019400980526    2C                   if        *inkl = *off
019500971015     c*
019600971028     c* per 71 on non leggo perche' non ci sono record
019700980601     c   71              readc     dn05s01                                30
019800980526    3c   71              dow       *in30 = *off
019900971016     c* imposto la key
020000980601    4c                   if        v5copz = '1'
020100980601     c     v5cfca        chain     fndfa01l                           31
020200980601     c                   movel     fndfa         O05rec
020300980601     c                   move      ' '           O05err
020400980601     c                   move      *blanks       O05msg
020500980526     c                   eval      *in30 = *on
020600980526     c                   eval      *inkl = *on
020700980526   x4c                   else
020800980601     c                   readc     dn05s01                                30
020900980526     c
021000980526    4c                   endif
021100980526    3C                   enddo
021200980526    2c                   endif
021300980526    1C                   enddo
021400971021     c**
021500971016     c                   endsr
021600980601     C*-------------------------------------------------------------------------
021700980601     c* SUB_SUCCE Routine di Ricerca Fase Successiva
021800980601     C*-------------------------------------------------------------------------
021900980601     c     SUB_SUCCE     BEGSR
022000980601     C*
022100980601     c     i05fca        setgt     fndfa01l
022200980601     c                   read      fndfa01l                               30
022300980601     c*
022400980605    1c                   dow       *in30 = *off
022500980601     c* controllo che :
022600980617     c* record valido
022700980720    2c                   if        dfaatb = *blanks
022800980601     c* flag validita' sia come quello richiesto
022900980720    3c                   if        I05FPR = dfaval or
023000980605     c                             dfaval = ' '    or
023100980605     c                             i05fpr = ' '
023200980601     c* flag obbligatorietà sia come quello richiesto
023300980720    4c                   if        i05ffs = 'O' and dfaobl = 'S'
023400980601     c                             or i05ffs = 'F' and dfaobl = ' '
023500980605     c                             or i05ffs = ' '
023600981030     c* flag fase utilizzata in evento oppure no
023700980720    5c                   if        i05fpe = dfaene or
023800980605     c                             dfaene = ' '    or
023900980605     c                             i05fpe = ' '
024000981030     c* flag fase utilizzata in denuncia oppure no
024100981030    6c                   if        i05fde = dfafde or
024200981030     c                             dfafde = ' '    or
024300981030     c                             i05fde = ' '
024400980717     c* se mi hanno passato il tipo anomalia controllo se la fase e' valida
024500980717     c* aggancio il dettaglio fase con tipo anomalia blank (valida x tutti)
024600980717     c* e se non va bene aggancio con il tipo anomalia passato
024700981030    7c                   if        i05tad <> *blanks
024800980717     c* tipo anomalia a blank
024900980717     c                   move      *blanks       tad
025000980717     c     kdfd          chain     fndfd01l                           31
025100980717     c   31              move      i05tad        tad
025200980717     c   31kdfd          chain     fndfd01l                           31
025300981030    7c                   endif
025400981030    7c                   if        *in31 = *off
025500981125     c* se la fase che trovo ha dei giorni di attesa per essere eseguita controllo :
025600981125     c* il flag ricerca data ultima fase di riferimento nello storico fasi
025700981125    8c                   if        dfarud <> ' '
025800981125     c                   exsr      ric_ufa
025900981125    8c                   endif
026000981125     c* se la fase che trovo ha dei giorni di attesa per essere eseguita controllo
026100981125     c* se la fase di attesa è uguale a quella passata e se i giorni che intercor-
026200981125     c* rono dall'ultima fase ad oggi sono > o = ai giorni di attesa  e il flag
026300981125     c* ricerca ultima data nello storico fasi non esiste
026400981030    8c                   if        (dfagga > 0 and  dfafra = i05fca
026500981125     c                             and NUM_DAYS >= dfagga  and dfarud = ' ')
026600981125     c* oppure flag ricerca ultima data fase riferimento è valorizzato e
026700981125     c* i giorni che interrcorrono sono giusti
026800981125     c                             or (dfagga > 0 and dfarud <> ' ' and
026900981125     c                             NUM_GIOR >= dfagga)
027000980720     c* oppure non ci sono giorni di attesa
027100980720     c                             or dfagga = 0
027200980721     c                   else
027300980721     c* metto la N di fase non eseguibile
027400980721     c                   move      'N'           O05ese
027500981030    8c                   endif
027600980601     c* valorizzo il campo del P.O. abilitato nella fase successiva se non e'
027700980601     c* già valorizzato per questa fase
027800981030    8c                   if        dfagfs = ' ' and i05tpc = 'M'
027900980601     c                   eval               dfagfs = 'P'
028000981030    8c                   endif
028100980601     c*
028200981030    8c                   if        dfagfs = ' ' and i05tpc = 'D'
028300980601     c                   eval               dfagfs = 'A'
028400981030    8c                   endif
028500980601     c*
028600980601     c* valorizzo la DS e passo il record e vado a fine lettura
028700980602     c                   movel     fndfa         O05rec
028800980601     c                   eval      *in30 = *on
028900981030    7c                   endif
029000980721    6c                   endif
029100980721    5c                   endif
029200980721    4c                   endif
029300980721    3c                   endif
029400980721    2c                   endif
029500980601     c*
029600980601     c  n30              read      fndfa01l                               30
029700980601     c*
029800980605    1c                   enddo
029900980605     c*
030000980605     c                   if        o05rec = *blanks
030100980605     c                   move      'E'           O05err
030200980605     c                   move      msg(5)        O05msg
030300980605     c                   endif
030400980601     c*
030500980601     c                   endsr
030600981125     c*-------------------------------------------------------------------------
030700981125     c* Routine ricerca ultima data esecuzi9one fase di riferimento calcolo giorni
030800981125     c*-------------------------------------------------------------------------
030900981125     c     ric_ufa       begsr
031000981125     c*
031100981125     c                   z-add     *zeros        num_gior
031200981126     c                   clear                   okfase
031300981125     c*
031400981125     c     kdcf          setgt     fndcf01l
031500981125     c     kdcf          readpe    fndcf01l                               32
031600981125     c*
031700981125     c                   dow       *in32 = *off
031800981125     c* controllo se si tratta della fase in questione
031900981125     c                   if        dcffca = dfafra
032000981126     c* ho trovato la fase
032100981126     c                   eval      okfase = 'S'
032200981125     c* calcolo i giorni
032300981125     c     *iso          move      dcfdfc        Wdataeur
032400981125     c     wdataoggi     subdur    wdataeur      num_gior:*D       5 0
032500981125     c* esco
032600981125     c                   leave
032700981125     c*
032800981125     c                   endif
032900981125     c     kdcf          readpe    fndcf01l                               32
033000981125     c                   enddo
033100981126     c* se non ho trovato fasi metto a 99999 il numero dei giorni intercorsi
033200981126     c* perchè vuol dire che:
033300981126     c* la fase che sto leggendo deve essere obbligatoria ed eseguibile solo
033400981126     c* dopo tot giorni da un'altra fase obbligatoria che però non è mai stata
033500981126     c* eseguita... perchè non era obbligatoria per quel tipo di CA. ESEMPIO :
033600981126     c* dovrebbe essere solo il caso della conferma CA dopo 15 GG lettera reso per
033700981126     c* avarie per una CA con tipo anomalia diversa da avaria e quindi senza
033800981126     c* l'obbligo di stampare la lettera ...
033900981126     c                   if        okfase = ' '
034000981126     c                   z-add     99999         num_gior
034100981126     c                   endif
034200981125     c*
034300981125     c                   endsr
034400981125     c*
034500981125     c*-------------------------------------------------------------------------
034600981125     c* Routine iniziale
034700981125     c*-------------------------------------------------------------------------
034800981125     c     *inzsr        begsr
034900981125     c*
035000981125     C     *ENTRY        PLIST
035100981125     C                   PARM                    KPJBA
035200981125     c                   parm                    fidn05
035300981125     C                   Z-ADD     1             CODUT
035400981125     C                   CALL      'XPARUT'
035500981125     C                   PARM                    PARUT
035600981125     C                   MOVEL     RAGUT         RSUT             20
035700981125      * ORA
035800981125     C                   TIME                    W0140
035900981125      * UDATE IN GGMMAAAA
036000981125     C                   MOVE      W0140         WDTGIO
036100981125      * UDATE IN AAAAMMGG
036200981125     C                   Z-ADD     WDTGIO        G02DAT
036300981125     C                   MOVEL     *BLANK        G02ERR
036400981125     C                   CALL      'XSRDA8'
036500981125     C                   PARM                    WLBDAT
036600981125     C                   MOVEL     G02INV        DATEU
036700981125     C     *iso          MOVEL     DATEU         Wdataoggi
036800981125      * calcolo i giorni che sono passati dall'ultima fase
036900981125     c                   if        i05dta  >0
037000981125     c     *iso          move      i05dta        Wdataeur
037100981125     c     wdataoggi     subdur    wdataeur      num_days:*D       5 0
037200981125     c                   else
037300981125     c                   z-add     *zeros        num_days
037400981125     c                   endif
037500981125     c*
037600981125     c     kdfd          klist
037700981125     c                   kfld                    dfafca
037800981125     c                   kfld                    tad
037900981125     c* chiave ricerca storico fasi CA
038000981125     c     kdcf          klist
038100981125     c                   kfld                    kaac
038200981125     c                   kfld                    kfil
038300981125     c                   kfld                    knca
038400981125     c*
038500981125     c* valorizzo i campi chiave
038600981125     c                   eval      kaac = i05aac
038700981125     c                   eval      kfil = i05fil
038800981125     c                   eval      knca = i05nca
038900981125     c*
039000990208     c* se entro in modo ricerca apro il file video
039100990208     c*
039200990208     c                   if        i05mod = 'R'
039300990208     c                   open      fidn05d
039400990208     c                   endif
039500990208     c*
039600981125     c                   endsr
039700980526**
039800980601Fase inesistente
039900980601Fase annullato
040000980601Errore non trovati dati per questa FASE
040100980526Non e' stato selezionato nulla
040200980605Non trovata fase successiva
