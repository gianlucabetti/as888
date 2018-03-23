000100030120      * TRTRN2R *-----------------------------------------------------*
000200030120      *         - RICEZIONE FIAR5 IN SEDE -                           *
000300030120      *---------------------------------------------------------------*
000400030120
000500030120      *****************************************************************
000600030120      *  RIEPILOGO INDICATORI                                         *
000700030120      *****************************************************************
000800030120      * 20-22 - Tipo record letto dal file TITAS30C                   *
000900030129      * 30    - Comodo
001000030120      * 50-51 - Tipo record letto dal file FIRA530C                   *
001100030120      * 97    - Membro non disponibile                                *
001200030120      *****************************************************************
001300030120
001400030120     h decedit('0,') datedit(*dmy.)
001500030120
001600030120      *---------------------------------------------------------------*
001700030120
001800030120     fTITAS30C  If   e           k disk
001900030129     fTNTBE01L  If   e           k disk
002000030129     fTABEL00F  If   e           k disk
002100030129      *
002200030120     fFIAR500R  Uf   e             disk    usropn
002300030120     f                                     rename(FIAR5000:FIAR50R)
002400030120     f                                     prefix(R_)
002500040531     fFIAR531C  Uf a e           k disk
002600150212     fazFGB02L  If   e           k disk
002700030120
002800030120      *---------------------------------------------------------------*
002900030120
003000030120      * Costanti :
003100030120      *
003200030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003300030120      * Schiere :
003400030129      *
003500030129     d AR5n            s                   like(AR5trd)  dim(50)
003600030129     d TBf             s                   like(TAStbl)  dim(50)
003700030120      *
003800030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003900030120      * Strutture Dati :
004000030129      *
004100030129     d Status         sds           333
004200030129     d  SDSpgm           *proc
004300030120      *
004400030120     d KPJBA         e ds                                                       *architettura
004500030120     d  LEGfil               483    492                                          -libreria file
004600030120     d  MBRfil               493    502                                          -membro file
004700030129      *
004800030129     d UT§DSE0F      e ds
004900030129     d CNCR80        e ds
005000030120      *
005100150212     d TISIG0DS      e ds
005200150212      *
005300030120     d dsFIAR5R      e ds                  extname(FIAR500R) inz
005400030120     d                                     prefix(R_)
005500030120     d dsFIAR5F      e ds                  extname(FIAR500F) inz
005600030129      *
005700030129     d dsTB          e ds                  inz
005800030129     d dAR5          e ds                  inz
005900030129     d dAR5ban       e ds                  inz
006000150212     d dAR5bnb       e ds                  inz
006100120405     d dAR5gen       e ds                  inz
006200030120      *
006300030120     d WLBDA8          ds                  inz                                  *ds "XSRDA8"
006400030120     d  G08dat                        8  0 inz                                   -data g/m/aa
006500030120     d  G08inv                        8  0 inz                                   -data aa/m/g
006600030120     d  G08err                        1    inz                                   -errore
006700030120     d  G08tgi                        5  0 inz                                   -n°giorni data
006800030130      *
006900030130     d dsRIFDOC        ds            14    inz
007000030130     d  TASlnp
007100030130     d  Wsep1                         1    inz('-')
007200030130     d  TASnrs
007300030130     d  Wsep2                         1    inz('-')
007400030130     d  TASnsp
007500030120      *
007600030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
007700030120      * Variabili
007800030120      *
007900030120      * - Campi di comodo
008000030801     d DataMax         s              8  0 Inz
008100030129     d SAVjba          s                   like(KPJBA) inz
008200030129     d Wtba            s              1  0 inz
008300030129     d Wa03            s              3    inz                                  *alfanum. 3
008400030801     d Wiso            s               d   datfmt(*iso)
008500030120     d Wn14            s             14  0                                      *numerico 14
008600030120     d Wn08            s              8  0                                      *numerico 8
008700030801     d Wnotitas        s              1
008800030120     d Woggi           s              8  0                                      *d.corrente aa/m/g
008900030129     d Wora            s              6  0                                      *h.corrente h:m:s
009000030129      * - Flag
009100030129     d $PFranco        s              1    inz(*off)
009200030129      * - Indici di schiera
009300030129     d xx              s              3  0 inz
009400030129      * - Contatori record estratti
009500030120     d $Rec            s              7  0 inz                                  *rec ricevuti
009600030129     d $RecWFM         s              7  0 inz
009700030120
009800030120      *---------------------------------------------------------------*
009900030120
010000030120     iTITAS000      20
010100030120     iTITAS010      21
010200030120     iTITASP00      22
010300030120      *
010400030120     iFIAR5000      50
010500030120     iFIAR5P00      51
010600030120
010700030120      *---------------------------------------------------------------*
010800030120
010900030120      * Verifica l'esistenza del membro di ricezione
011000030120     c                   exsr      EsiMBR
011100030120     c   97              goto      Fine
011200030120      *
011300030120      * Operazioni di ricezione
011400030120     c                   exsr      RicMBR
011500030120      *
011600030120      * Disalloca il membro di ricezione
011700030120     c                   exsr      DisMBR
011800030120      * Fine
011900030120     c     Fine          tag
012000030120     c                   eval      *inLR = *on
012100030120
012200030120      **-------------------------------------------------------------**
012300030120      ** Operazioni iniziali                                         **
012400030120      **-------------------------------------------------------------**
012500030120     c     *InzSR        BEGSR
012600030120      *
012700030120     c     *Entry        Plist
012800030120     c                   parm                    KPJBA
012900030120      *
013000030120      * Chiavi di lettura:
013100040531      * - FIAR531C > completa (8 campi chiave)
013200030120     c     K08AR501      klist
013300030120     c                   kfld                    r_AR5aas
013400030120     c                   kfld                    r_AR5lnp
013500030120     c                   kfld                    r_AR5nrs
013600030120     c                   kfld                    r_AR5nsp
013700030120     c                   kfld                    r_AR5trd
013800040524     c                   kfld                    r_AR5prg
013900030120     c                   kfld                    r_AR5dac
014000030120     c                   kfld                    r_AR5orc
014100030120      * - TITAS30C > INcompleta (4 campi chiave)
014200030120     c     K04TAS30      klist
014300030120     c                   kfld                    r_AR5aas
014400030120     c                   kfld                    r_AR5lnp
014500030120     c                   kfld                    r_AR5nrs
014600030120     c                   kfld                    r_AR5nsp
014700030129      * - TABEL00F > INcompleta (2 campi chiave)
014800030129     c     K02TABEL      klist
014900030129     c                   kfld                    TBLkut
015000030129     c                   kfld                    TBLcod
015100030129      *
015200030129      * reperisco i dati aziendali
015300030129     c                   call      'X§PARUT'
015400030129     c                   parm                    UT§DSE0F
015500030129     c                   movel     Rec80         CNCR80
015600030120      *
015700030120      * calcola la data corrente
015800030120     c                   time                    Wn14                           *ora(6) + data(8)
015900030129     c                   movel     Wn14          Wora                           *ora(6)
016000030120     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
016100030120     c                   z-add     Wn08          G08dat
016200030120     c                   clear                   G08inv
016300030120     c                   movel     *off          G08err
016400030120     c                   call      'XSRDA8'
016500030120     c                   parm                    WLBDA8
016600030120     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
016700030801      * Data corrente iso
016800030801     c     *iso          Movel     Woggi         Wiso
016900030801      * Data max per cancellazione record già ricevuti
017000030801     c                   Subdur    10:*d         Wiso
017100030801     c                   Move      Wiso          DataMax
017200030120      *
017300030120      * se il membro da ricevere non è stato dichiarato,
017400030120      *   assume il primo del file
017500030120if  1c                   if        MBRfil = *blanks
017600030120     c                   eval      MBRfil = '*FIRST'
017700030120e   1c                   endif
017800030129      *
017900030129      * Effettuo caricamento dei tipi bolla con "Porto Franco"
018000030129     c                   clear                   xx
018100030129     c                   clear                   TBf
018200030129     c                   eval      TBLkut = 1
018300030129     c                   eval      TBLcod = 'TB'
018400030129     c     K02TABEL      setll     TABEL
018500030129     c     K02TABEL      reade     TABEL
018600030129do  1c                   DOW       Not %Eof(TABEL00F)
018700030129if  2c                   IF        TBLflg = *blanks
018800030129     c                   eval      dsTB = TBLuni
018900030129if  3c                   If        §TBtpo = 'F'
019000030129     c                   add       1             xx
019100030129     c                   movel     TBLkey        TBf(xx)
019200030129e   3c                   Endif
019300030129e   2c                   ENDIF
019400030129     c     K02TABEL      reade     TABEL
019500030129e   1c                   ENDDO
019600030129      *
019700030129      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
019800030129     c                   clear                   xx
019900030129     c                   clear                   AR5n
020000030129     c                   eval      TBEcod = 'AR5'
020100030129     c     TBEcod        setll     TNTBE000
020200030129     c     TBEcod        reade     TNTBE000
020300030129do  1c                   DOW       Not %Eof(TNTBE01L)
020400030129if  2c                   IF        TBEatb = *blanks
020500030129     c                   eval      dAR5 = TBEuni
020600030129if  3c                   If        §AR5trmi <> 'S'
020700030129     c                   add       1             xx
020800030129     c                   movel     TBEke1        AR5n(xx)
020900030129e   3c                   Endif
021000030129e   2c                   ENDIF
021100030129     c     TBEcod        reade     TNTBE000
021200030129e   1c                   ENDDO
021300030120      *
021400030120     c                   ENDSR
021500030120      **-------------------------------------------------------------**
021600030120      ** Verifica l'esistenza del membro di ricezione                **
021700030120      **-------------------------------------------------------------**
021800030120     c     EsiMBR        BEGSR
021900030120      *
022000030129      * File FIAR500R:
022100030129     c                   eval      UL25pf = 'FIAR500R  '
022200030129      *
022300030120      * controlla esistenza membro di ricezione,
022400030120      *   se non esiste (*in97 = *on) chiude il programma
022500030120     c                   eval      UL25tc = '2'
022600030120     c                   exsr      CallTRUL25
022700030120     c   97              goto      F_EsiMBR
022800030120      *
022900030120      * alloca il membro di ricezione,
023000030120      *   se non riesce (*in97 = *on) chiude il programma
023100030120     c                   eval      UL25tc = '4'
023200030120     c                   exsr      CallTRUL25
023300030120     c   97              goto      F_EsiMBR
023400030120      *
023500030120      * esegue sovrapposizione membro di ricezione,
023600030120      *   se non riesce (*in97 = *on) chiude il programma
023700030120     c                   eval      UL25tc = '5'
023800030120     c                   exsr      CallTRUL25
023900030120     c   97              goto      F_EsiMBR
024000030120      *
024100030120     c     F_EsiMBR      ENDSR
024200030120
024300030120      **-------------------------------------------------------------**
024400030120      ** Richiamo cl TRUL25C                                         **
024500030120      **-------------------------------------------------------------**
024600030120     c     CallTRUL25    BEGSR
024700030120      *
024800030120     c                   call      'TRUL25C'                            97
024900030120     c                   parm                    UL25tc            1
025000030129     c                   parm                    UL25pf           10
025100030120     c                   parm      *blanks       UL25li           10
025200030120     c                   parm      MBRfil        UL25mb           10
025300030120     c                   parm      *blanks       UL25tx           10
025400030120     c                   parm      *blanks       UL25fr            1
025500030120     c                   parm      *blanks       UL25os            1
025600030120     c                   parm      *zeros        UL25es            2
025700030120      *
025800030120     c  N97UL25es        comp      *zeros                             9797
025900030120      *
026000030120     c                   ENDSR
026100030120      **-------------------------------------------------------------**
026200030120      ** Operazioni di ricezione                                     **
026300030120      **-------------------------------------------------------------**
026400030120     c     RicMBR        BEGSR
026500030120      *
026600030120      * apre il file di ricezione
026700030120     c                   open      FIAR500R
026800030120      *
026900030120      * legge il file di ricezione
027000030120     c                   read      FIAR50R
027100030120do  1c                   dow       Not %eof(FIAR500R)
027200030120      *
027300030120      * scrive il record fisico
027400030120     c                   exsr      ScrPFm
027500030120      * cancella il record di ricezione
027600030801      * sempre se trovato Titas
027700030801If  1c                   If        Wnotitas = *Blanks
027800030120     c                   delete    FIAR50R
027900030801   x1c                   Else
028000030801      * se sono passati tot. giorni dalla creazione del rcd se non trovato Titas
028100030910If  2c                   If        r_Ar5Dac <= DataMax
028200030801     c                   Delete    FIAR50R
028300030801    2c                   EndIf
028400030801    1c                   EndIf
028500030120      *
028600030120     c                   read      FIAR50R
028700030120e   1c                   enddo                                                  *fine file ricezione
028800030120      *
028900030120      * chiude il file di ricezione
029000030120     c                   close     FIAR500R
029100030120      *
029200030120     c                   ENDSR
029300030120      **-------------------------------------------------------------**
029400030120      ** Scrive il record fisico                                     **
029500030120      **-------------------------------------------------------------**
029600030120     c     ScrPFm        BEGSR
029700030120      *
029800030801     c                   Clear                   Wnotitas
029900030120      * controlla se esiste la bolla:
030000030120      * carico estensioni solo se esiste la testata
030100030120     c                   movea     '000'         *in(20)
030200030120     c     K04TAS30      chain     TITAS30C
030300030120if  1c                   if        %found(TITAS30C)
030400030120      *
030500030120      * aggancia il record da aggiornare
030600030120     c                   movea     '00'          *in(50)
030700040531     c     K08AR501      chain     FIAR531C
030800120405     c                   clear                   dar5gen
030900120405if  2c                   if        %found(FIAR531C)  and ar5trd='GEN'
031000120405     c                   eval      dar5gen=ar5uni
031100120405     c                   endif
031200030120      *
031300030120      * imposta i campi del file fisico con quelli del record ricevuto
031400030120     c                   movel     dsFIAR5r      dsFIAR5f                       *formato record
031500120405     c
031600120405if  2c                   if        %found(FIAR531C)  and ar5trd='GEN'
031700120405     c
031800120405     c* risposto i campi solo SEDE per evitare che si abblankino
031900120405     c                   eval      %subst(ar5uni:107:8)=§ar5drdt
032000120405     c                   eval      %subst(ar5uni:115:1)=§AR5FSTD
032100120405     c                   eval      %subst(ar5uni:167:8)=§AR5MTVDT
032200120405     c                   eval      %subst(ar5uni:175:8)=§AR5DIMTV
032300120405     c                   endif
032400030120      *
032500030130      * - fleggo come ricevuto in filiale
032600030130     C                   eval      AR5ft1 = 'R'
032700030130     C                   eval      AR5dt1 = Woggi
032800030129      * - fleggo come ricevuto in sede
032900030129     c                   eval      AR5ft2 = 'R'
033000030129     c                   eval      AR5dt2 = Woggi
033100030129      * - verifico se per il tipo record di FIAR5 è previsto il
033200030129      *   ritorno info al cliente: se non è previsto imposto
033300030129      *   il record come già trasmesso (cioè da non trasmettere).
033400030129     c                   eval      *in30  =  *off
033500030129     c     AR5trd        lookup    AR5n                                   30
033600030129if  2c                   IF        *in30
033700030129     c                   eval      AR5ft3 = 'T'
033800030129     c                   eval      AR5dt3 = *all'9'
033900030129x   2c                   ELSE
034000030129     c                   clear                   AR5ft3
034100030129     c                   clear                   AR5dt3
034200030129e   2c                   ENDIF
034300030120      *
034400040531if  2c                   if        Not %found(FIAR531C)
034500040308      * se record 'GEN' a ar5uni è vuoto non devo scriverlo
034600040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
034700040308    3c                   Else
034800030120      * inesistente, inserimento
034900030120     c   20              write     FIAR5000
035000030120     c   21              write     FIAR5000
035100030120     c   22              write     FIAR5P00
035200040308    3c                   EndIf
035300030120x   2c                   else
035400040308      * se record 'GEN' a ar5uni è vuoto devo deletare il record
035500040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
035600040308     c   50              delete    FIAR5000
035700040308     c   51              delete    FIAR5P00
035800040308    3c                   Else
035900030120      * esistente, aggiornamento
036000030120     c   50              update    FIAR5000
036100030120     c   51              update    FIAR5P00
036200040308    3c                   EndIf
036300030120e   2c                   endif
036400030129      *
036500030120      *
036600030801   x2c                   Else
036700030801     c                   Eval      wnotitas = '1'
036800030120e   1c                   endif
036900030120      *
037000150212      * x record di tipo "BAN" o "BNB" ricevuti in FIAR5
037100150212if  2c                   if        AR5trd = 'BAN' or AR5trd = 'BNB'
037200150212      * registrazione movimenti Bancali
037300150212     c                   exsr      x_Bancali
037400150212e   2c                   endif
037500150212      *
037600030120      * conteggio dei record ricevuti
037700030120      *   (anche se non esiste più la testata, quindi come se non fosse
037800030120      *   stato "ricevuto")
037900030120     c                   add       1             $Rec                           *n° record ricevuti
038000030120      *
038100030120     c                   ENDSR
038200030129
038300030120      **-------------------------------------------------------------**
038400030120      ** Disalloca il membro di ricezione                            **
038500030120      **-------------------------------------------------------------**
038600030120     c     DisMBR        BEGSR
038700030120      *
038800030120      * cancella la sovrascrittura del membro di ricezione   e
038900030129      * disalloca il membro di ricezione - file FIAR500R
039000030129     c                   eval      UL25pf = 'FIAR500R  '
039100030120     c                   eval      UL25tc = '9'
039200030120     c                   exsr      CallTRUL25
039300030120      *
039400030120     c                   ENDSR
039500150212     C*--------------------------------------------------------------------------------------------*
039600150212     C* considerazioni per gestione Bancali nelle spedizioni
039700150212     C*--------------------------------------------------------------------------------------------*
039800150212     C     x_Bancali     BEGSR
039900150212      *
040000150212      * Verifico se la spedizione è a "Porto Franco"
040100150212      * (mi servirà per stabilire il codice soggetto WFMSOG)
040200150212     c                   eval      *in30  =  *off
040300150212     c     TAStbl        lookup    TBf                                    30
040400150212     c                   eval      $PFranco = *in30
040500150212      *
040600150212     c                   move      *zero         codFiglio         8
040700150212sel 1c                   select
040800150212w   1c                   when      TASccm <> *zeros
040900150212     c                   move      TASccm        codFiglio
041000150212w   1c                   when      $PFranco = *on
041100150212     c                   move      TASksc        codFiglio
041200150212e   1c                   endsl
041300150212      *
041400150212      *   se il cliente è iscritto nell'anagrafica
041500150212      *   x la gestione dei Bancali  allora deve scrivere il movimento
041600150212     c     codFiglio     chain     azFGB02L
041700150212     c                   if        %Found(azFGB02L) and fgbANN =' '
041800150212      *
041900150212      * scrive il record Movimento dei bancali
042000150212     C                   CLEAR                   TISIG0DS
042100150212      *
042200150212      *  x  Tipo Bancale: BAN
042300150212if  2c                   if               AR5trd = 'BAN'
042400150212     c                   movel     AR5uni        dAR5ban
042500150212     C                   MOVEL     'RTMA'        IG0CAU
042600150212      *  se però è un RESO la causale è :
042700150212     c                   if        tasFBR ='S'
042800150212     C                   MOVEL     'REME'        IG0CAU
042900150212     c                   end
043000150212      *  1° bancale
043100150212if  1c                   if        §AR5nba > *zeros
043200150212     c                   movel(p)  §AR5tba       IG0COD
043300150212     C                   Z-ADD     §AR5nba       IG0QTA
043400150212     C                   EXSR      scrpfmov
043500150212e   1c                   endif
043600150212      *  2° bancale
043700150212if  1c                   if        §AR5nb2 > *zeros
043800150212     c                   movel(p)  §AR5tb2       IG0COD
043900150212     C                   Z-ADD     §AR5nb2       IG0QTA
044000150212     C                   EXSR      scrpfmov
044100150212e   1c                   endif
044200150212      *
044300150212      *  x  Tipo Bancale: BNB
044400150212if  2c                   elseIf           AR5trd = 'BNB'
044500150212     c                   movel     AR5uni        dAR5bnb
044600150212     C                   MOVEL     'TPU '        IG0CAU
044700150212      *  1° bancale
044800150212if  1c                   if        §AR5Bnba > *zeros
044900150212     c                   movel(p)  §AR5Btba      IG0COD
045000150212     C                   Z-ADD     §AR5Bnba      IG0QTA
045100150212     C                   EXSR      scrpfmov
045200150212e   1c                   endif
045300150212      *  2° bancale
045400150212if  1c                   if        §AR5Bnb2 > *zeros
045500150212     c                   movel(p)  §AR5Btb2      IG0COD
045600150212     C                   Z-ADD     §AR5Bnb2      IG0QTA
045700150212     C                   EXSR      scrpfmov
045800150212e   1c                   endif
045900150212      *
046000150212     c                   end
046100150212      *
046200150212e   2c                   endif
046300150212      *
046400150212     c                   ENDSR
046500150212     C*--------------------------------------------------------------------------------------------*
046600150212     C* scrpfMOV - scrive il record fisico
046700150212     C*--------------------------------------------------------------------------------------------*
046800150212     C     scrpfmov      BEGSR
046900150212     C*
047000150212     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento
047100150212     C                   Z-ADD     wOGGI         IG0DAT
047200150212     C                   Z-ADD     wORA          IG0ORA
047300150212     C                   MOVEL     KNMUS         IG0UTE
047400150212     C                   Z-ADD     SIMpou        IG0POU
047500150212     C                   movel     SDSpgm        IG0PGM
047600150212     C                   MOVEL     fgbKSU        IG0SOG
047700150212     C                   eval      IG0RDO = dsRIFDOC
047800150212     C                   eval      IG0DDO = (TASaas * 10000) + TASmgs
047900150212     C*
048000150212     C* Lancio del "driver"
048100150212     C                   CALL(e)   'TISIG5R'
048200150212     C                   PARM                    TISIG0DS
048300150212     C*
048400150212     C* Se errore in chiamata "driver" gestisco come se restituito errore dal "driver" stesso
048500150212     C                   IF        %error
048600150212     C                   EVAL      OG0ERR = 'E'
048700150212     C                   EVAL      OG0MSG = 'ERRORE IN CHIAMATA DRIVER'
048800150212     C                   ENDIF
048900150212     C*
049000150212     C                   ENDSR
049100150212     C*--------------------------------------------------------------------------------------------*
