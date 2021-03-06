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
002200030120     fCMRIC01L  Uf   e           k disk
002300030120     fFIAR500R  Uf   e             disk    usropn
002400030120     f                                     rename(FIAR5000:FIAR50R)
002500030120     f                                     prefix(R_)
002600040531     fFIAR531C  Uf a e           k disk
002700030129     fWFMGB01R  Uf a e           k disk    usropn
002800030120
002900030120      *---------------------------------------------------------------*
003000030120
003100030120      * Costanti :
003200030120      *
003300030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003400030120      * Schiere :
003500030129      *
003600030129     d AR5n            s                   like(AR5trd)  dim(50)
003700030129     d TBf             s                   like(TAStbl)  dim(50)
003800030120      *
003900030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004000030120      * Strutture Dati :
004100030129      *
004200030129     d Status         sds           333
004300030129     d  SDSpgm           *proc
004400030120      *
004500030120     d KPJBA         e ds                                                       *architettura
004600030120     d  LEGfil               483    492                                          -libreria file
004700030120     d  MBRfil               493    502                                          -membro file
004800030129      *
004900030129     d UT�DSE0F      e ds
005000030129     d CNCR80        e ds
005100030120      *
005200030120     d dsFIAR5R      e ds                  extname(FIAR500R) inz
005300030120     d                                     prefix(R_)
005400030120     d dsFIAR5F      e ds                  extname(FIAR500F) inz
005500030129      *
005600030129     d dsTB          e ds                  inz
005700030129     d dAR5          e ds                  inz
005800030129     d dAR5ban       e ds                  inz
005900030120      *
006000030120     d WLBDA8          ds                  inz                                  *ds "XSRDA8"
006100030120     d  G08dat                        8  0 inz                                   -data g/m/aa
006200030120     d  G08inv                        8  0 inz                                   -data aa/m/g
006300030120     d  G08err                        1    inz                                   -errore
006400030120     d  G08tgi                        5  0 inz                                   -n�giorni data
006500030130      *
006600030130     d dsRIFDOC        ds            14    inz
006700030130     d  TASlnp
006800030130     d  Wsep1                         1    inz('-')
006900030130     d  TASnrs
007000030130     d  Wsep2                         1    inz('-')
007100030130     d  TASnsp
007200030120      *
007300030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
007400030120      * Variabili
007500030120      *
007600030120      * - Campi di comodo
007700030801     d DataMax         s              8  0 Inz
007800030129     d SAVjba          s                   like(KPJBA) inz
007900030129     d Wtba            s              1  0 inz
008000030129     d Wa03            s              3    inz                                  *alfanum. 3
008100030801     d Wiso            s               d   datfmt(*iso)
008200030120     d Wn14            s             14  0                                      *numerico 14
008300030120     d Wn08            s              8  0                                      *numerico 8
008400030801     d Wnotitas        s              1
008500030120     d Woggi           s              8  0                                      *d.corrente aa/m/g
008600030129     d Wora            s              6  0                                      *h.corrente h:m:s
008700030129      * - Flag
008800030129     d $PFranco        s              1    inz(*off)
008900030129      * - Indici di schiera
009000030129     d xx              s              3  0 inz
009100030129      * - Contatori record estratti
009200030120     d $Rec            s              7  0 inz                                  *rec ricevuti
009300030129     d $RecWFM         s              7  0 inz
009400030120
009500030120      *---------------------------------------------------------------*
009600030120
009700030120     iTITAS000      20
009800030120     iTITAS010      21
009900030120     iTITASP00      22
010000030120      *
010100030120     iFIAR5000      50
010200030120     iFIAR5P00      51
010300030120
010400030120      *---------------------------------------------------------------*
010500030120
010600030120      * Verifica l'esistenza del membro di ricezione
010700030120     c                   exsr      EsiMBR
010800030120     c   97              goto      Fine
010900030120      *
011000030120      * Operazioni di ricezione
011100030120     c                   exsr      RicMBR
011200030120      *
011300030120      * Disalloca il membro di ricezione
011400030120     c                   exsr      DisMBR
011500030129      *
011600030129      * Se ho estratto dei dati in WFMGB00R, richiamo l'azione di
011700030129      * ricezione movimenti bancali
011800030129if  1c                   if        $RecWFM > 0
011900030129     c                   exsr      CallTISIG6
012000030129e   1c                   endif
012100030120      *
012200030120      * Fine
012300030120     c     Fine          tag
012400030120     c                   eval      *inLR = *on
012500030120
012600030120      **-------------------------------------------------------------**
012700030120      ** Operazioni iniziali                                         **
012800030120      **-------------------------------------------------------------**
012900030120     c     *InzSR        BEGSR
013000030120      *
013100030120     c     *Entry        Plist
013200030120     c                   parm                    KPJBA
013300030120      *
013400030120      * Chiavi di lettura:
013500040531      * - FIAR531C > completa (8 campi chiave)
013600030120     c     K08AR501      klist
013700030120     c                   kfld                    r_AR5aas
013800030120     c                   kfld                    r_AR5lnp
013900030120     c                   kfld                    r_AR5nrs
014000030120     c                   kfld                    r_AR5nsp
014100030120     c                   kfld                    r_AR5trd
014200040524     c                   kfld                    r_AR5prg
014300030120     c                   kfld                    r_AR5dac
014400030120     c                   kfld                    r_AR5orc
014500030129      * - WFMGB01R > completa (9 campi chiave)
014600030129     c     K09WFM01      klist
014700030129     c                   kfld                    WFMdat
014800030129     c                   kfld                    WFMora
014900030129     c                   kfld                    WFMute
015000030129     c                   kfld                    WFMpou
015100030129     c                   kfld                    WFMpgm
015200030129     c                   kfld                    WFMcau
015300030129     c                   kfld                    WFMsog
015400030129     c                   kfld                    WFMcod
015500030129     c                   kfld                    WFMsmc
015600030120      * - TITAS30C > INcompleta (4 campi chiave)
015700030120     c     K04TAS30      klist
015800030120     c                   kfld                    r_AR5aas
015900030120     c                   kfld                    r_AR5lnp
016000030120     c                   kfld                    r_AR5nrs
016100030120     c                   kfld                    r_AR5nsp
016200030129      * - TABEL00F > INcompleta (2 campi chiave)
016300030129     c     K02TABEL      klist
016400030129     c                   kfld                    TBLkut
016500030129     c                   kfld                    TBLcod
016600030129      *
016700030129      * reperisco i dati aziendali
016800030129     c                   call      'X�PARUT'
016900030129     c                   parm                    UT�DSE0F
017000030129     c                   movel     Rec80         CNCR80
017100030120      *
017200030120      * calcola la data corrente
017300030120     c                   time                    Wn14                           *ora(6) + data(8)
017400030129     c                   movel     Wn14          Wora                           *ora(6)
017500030120     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
017600030120     c                   z-add     Wn08          G08dat
017700030120     c                   clear                   G08inv
017800030120     c                   movel     *off          G08err
017900030120     c                   call      'XSRDA8'
018000030120     c                   parm                    WLBDA8
018100030120     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
018200030801      * Data corrente iso
018300030801     c     *iso          Movel     Woggi         Wiso
018400030801      * Data max per cancellazione record gi� ricevuti
018500030801     c                   Subdur    10:*d         Wiso
018600030801     c                   Move      Wiso          DataMax
018700030120      *
018800030120      * se il membro da ricevere non � stato dichiarato,
018900030120      *   assume il primo del file
019000030120if  1c                   if        MBRfil = *blanks
019100030120     c                   eval      MBRfil = '*FIRST'
019200030120e   1c                   endif
019300030129      *
019400030129      * Effettuo caricamento dei tipi bolla con "Porto Franco"
019500030129     c                   clear                   xx
019600030129     c                   clear                   TBf
019700030129     c                   eval      TBLkut = 1
019800030129     c                   eval      TBLcod = 'TB'
019900030129     c     K02TABEL      setll     TABEL
020000030129     c     K02TABEL      reade     TABEL
020100030129do  1c                   DOW       Not %Eof(TABEL00F)
020200030129if  2c                   IF        TBLflg = *blanks
020300030129     c                   eval      dsTB = TBLuni
020400030129if  3c                   If        �TBtpo = 'F'
020500030129     c                   add       1             xx
020600030129     c                   movel     TBLkey        TBf(xx)
020700030129e   3c                   Endif
020800030129e   2c                   ENDIF
020900030129     c     K02TABEL      reade     TABEL
021000030129e   1c                   ENDDO
021100030129      *
021200030129      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
021300030129     c                   clear                   xx
021400030129     c                   clear                   AR5n
021500030129     c                   eval      TBEcod = 'AR5'
021600030129     c     TBEcod        setll     TNTBE000
021700030129     c     TBEcod        reade     TNTBE000
021800030129do  1c                   DOW       Not %Eof(TNTBE01L)
021900030129if  2c                   IF        TBEatb = *blanks
022000030129     c                   eval      dAR5 = TBEuni
022100030129if  3c                   If        �AR5trmi <> 'S'
022200030129     c                   add       1             xx
022300030129     c                   movel     TBEke1        AR5n(xx)
022400030129e   3c                   Endif
022500030129e   2c                   ENDIF
022600030129     c     TBEcod        reade     TNTBE000
022700030129e   1c                   ENDDO
022800030120      *
022900030120     c                   ENDSR
023000030120
023100030120      **-------------------------------------------------------------**
023200030120      ** Verifica l'esistenza del membro di ricezione                **
023300030120      **-------------------------------------------------------------**
023400030120     c     EsiMBR        BEGSR
023500030120      *
023600030129      * File FIAR500R:
023700030129     c                   eval      UL25pf = 'FIAR500R  '
023800030129      *
023900030120      * controlla esistenza membro di ricezione,
024000030120      *   se non esiste (*in97 = *on) chiude il programma
024100030120     c                   eval      UL25tc = '2'
024200030120     c                   exsr      CallTRUL25
024300030120     c   97              goto      F_EsiMBR
024400030120      *
024500030120      * alloca il membro di ricezione,
024600030120      *   se non riesce (*in97 = *on) chiude il programma
024700030120     c                   eval      UL25tc = '4'
024800030120     c                   exsr      CallTRUL25
024900030120     c   97              goto      F_EsiMBR
025000030120      *
025100030120      * esegue sovrapposizione membro di ricezione,
025200030120      *   se non riesce (*in97 = *on) chiude il programma
025300030120     c                   eval      UL25tc = '5'
025400030120     c                   exsr      CallTRUL25
025500030120     c   97              goto      F_EsiMBR
025600030129      *
025700030129      *
025800030129      * File WFMGB01R:
025900030129     c                   eval      UL25pf = 'WFMGB01R  '
026000030129      *
026100030129      * aggiungo (eventualmente) il membro al file logico
026200030129     c                   call      'TRUL50C'                            97
026300030129     c                   parm      'WFMGB00R  '  UL50pf           10
026400030129     c                   parm      MBRfil        UL50pm           10
026500030129     c                   parm      KNSIF         UL50li           10
026600030129     c                   parm      'WFMGB01R  '  UL50lf           10
026700030129     c                   parm      MBRfil        UL50lm           10
026800030129     c                   parm      *blanks       UL50fl            1
026900030129     c   97              goto      F_EsiMBR
027000030129      *
027100030129      * alloca il membro di ricezione,
027200030129      *   se non riesce (*in97 = *on) chiude il programma
027300030129     c                   eval      UL25tc = '4'
027400030129     c                   exsr      CallTRUL25
027500030129     c   97              goto      F_EsiMBR
027600030129      *
027700030129      * esegue sovrapposizione membro di ricezione,
027800030129      *   se non riesce (*in97 = *on) chiude il programma
027900030129     c                   eval      UL25tc = '5'
028000030129     c                   exsr      CallTRUL25
028100030129     c   97              goto      F_EsiMBR
028200030120      *
028300030120     c     F_EsiMBR      ENDSR
028400030120
028500030120      **-------------------------------------------------------------**
028600030120      ** Richiamo cl TRUL25C                                         **
028700030120      **-------------------------------------------------------------**
028800030120     c     CallTRUL25    BEGSR
028900030120      *
029000030120     c                   call      'TRUL25C'                            97
029100030120     c                   parm                    UL25tc            1
029200030129     c                   parm                    UL25pf           10
029300030120     c                   parm      *blanks       UL25li           10
029400030120     c                   parm      MBRfil        UL25mb           10
029500030120     c                   parm      *blanks       UL25tx           10
029600030120     c                   parm      *blanks       UL25fr            1
029700030120     c                   parm      *blanks       UL25os            1
029800030120     c                   parm      *zeros        UL25es            2
029900030120      *
030000030120     c  N97UL25es        comp      *zeros                             9797
030100030120      *
030200030120     c                   ENDSR
030300030120
030400030120      **-------------------------------------------------------------**
030500030120      ** Operazioni di ricezione                                     **
030600030120      **-------------------------------------------------------------**
030700030120     c     RicMBR        BEGSR
030800030120      *
030900030120      * apre il file di ricezione
031000030120     c                   open      FIAR500R
031100030129      * apre il file per la movimentazione bancali
031200030129     c                   open      WFMGB01R
031300030120      *
031400030120      * legge il file di ricezione
031500030120     c                   read      FIAR50R
031600030120do  1c                   dow       Not %eof(FIAR500R)
031700030120      *
031800030120      * scrive il record fisico
031900030120     c                   exsr      ScrPFm
032000030120      * cancella il record di ricezione
032100030801      * sempre se trovato Titas
032200030801If  1c                   If        Wnotitas = *Blanks
032300030120     c                   delete    FIAR50R
032400030801   x1c                   Else
032500030801      * se sono passati tot. giorni dalla creazione del rcd se non trovato Titas
032600030910If  2c                   If        r_Ar5Dac <= DataMax
032700030801     c                   Delete    FIAR50R
032800030801    2c                   EndIf
032900030801    1c                   EndIf
033000030120      *
033100030120     c                   read      FIAR50R
033200030120e   1c                   enddo                                                  *fine file ricezione
033300030120      *
033400030120      * chiude il file di ricezione
033500030120     c                   close     FIAR500R
033600030129      * chiude il file per la movimentazione bancali
033700030129     c                   close     WFMGB01R
033800030120      *
033900030120      * aggiorna l'archivio di controllo delle ricezioni
034000030120     c                   exsr      AggCMR
034100030120      *
034200030120     c                   ENDSR
034300030120
034400030120      **-------------------------------------------------------------**
034500030120      ** Scrive il record fisico                                     **
034600030120      **-------------------------------------------------------------**
034700030120     c     ScrPFm        BEGSR
034800030120      *
034900030801     c                   Clear                   Wnotitas
035000030120      * controlla se esiste la bolla:
035100030120      * carico estensioni solo se esiste la testata
035200030120     c                   movea     '000'         *in(20)
035300030120     c     K04TAS30      chain     TITAS30C
035400030120if  1c                   if        %found(TITAS30C)
035500030120      *
035600030120      * aggancia il record da aggiornare
035700030120     c                   movea     '00'          *in(50)
035800040531     c     K08AR501      chain     FIAR531C
035900030120      *
036000030120      * imposta i campi del file fisico con quelli del record ricevuto
036100030120     c                   movel     dsFIAR5r      dsFIAR5f                       *formato record
036200030120      *
036300030130      * - fleggo come ricevuto in filiale
036400030130     C                   eval      AR5ft1 = 'R'
036500030130     C                   eval      AR5dt1 = Woggi
036600030129      * - fleggo come ricevuto in sede
036700030129     c                   eval      AR5ft2 = 'R'
036800030129     c                   eval      AR5dt2 = Woggi
036900030129      * - verifico se per il tipo record di FIAR5 � previsto il
037000030129      *   ritorno info al cliente: se non � previsto imposto
037100030129      *   il record come gi� trasmesso (cio� da non trasmettere).
037200030129     c                   eval      *in30  =  *off
037300030129     c     AR5trd        lookup    AR5n                                   30
037400030129if  2c                   IF        *in30
037500030129     c                   eval      AR5ft3 = 'T'
037600030129     c                   eval      AR5dt3 = *all'9'
037700030129x   2c                   ELSE
037800030129     c                   clear                   AR5ft3
037900030129     c                   clear                   AR5dt3
038000030129e   2c                   ENDIF
038100030120      *
038200040531if  2c                   if        Not %found(FIAR531C)
038300040308      * se record 'GEN' a ar5uni � vuoto non devo scriverlo
038400040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
038500040308    3c                   Else
038600030120      * inesistente, inserimento
038700030120     c   20              write     FIAR5000
038800030120     c   21              write     FIAR5000
038900030120     c   22              write     FIAR5P00
039000040308    3c                   EndIf
039100030120x   2c                   else
039200040308      * se record 'GEN' a ar5uni � vuoto devo deletare il record
039300040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
039400040308     c   50              delete    FIAR5000
039500040308     c   51              delete    FIAR5P00
039600040308    3c                   Else
039700030120      * esistente, aggiornamento
039800030120     c   50              update    FIAR5000
039900030120     c   51              update    FIAR5P00
040000040308    3c                   EndIf
040100030120e   2c                   endif
040200030129      *
040300030129      * Per ogni record di tipo "BAN" che scrivo in FIAR5
040400030129      *   ne scrivo 1 o 2 in WFMGB
040500030129if  2c                   if        AR5trd = 'BAN'
040600030129     c                   exsr      srWFMGB
040700030129e   2c                   endif
040800030120      *
040900030801   x2c                   Else
041000030801     c                   Eval      wnotitas = '1'
041100030120e   1c                   endif
041200030120      *
041300030120      * conteggio dei record ricevuti
041400030120      *   (anche se non esiste pi� la testata, quindi come se non fosse
041500030120      *   stato "ricevuto")
041600030120     c                   add       1             $Rec                           *n� record ricevuti
041700030120      *
041800030120     c                   ENDSR
041900030129
042000030129      **-------------------------------------------------------------**
042100030129      ** Scrivo/Aggiorno il MBR del file WFMGB00R (Movim. bancali)   **
042200030129      **-------------------------------------------------------------**
042300030129     c     srWFMGB       BEGSR
042400030129      *
042500030129      * Verifico se la spedizione � a "Porto Franco"
042600030129      * (mi servir� per stabilire il codice soggetto WFMSOG)
042700030129     c                   eval      *in30  =  *off
042800030129     c     TAStbl        lookup    TBf                                    30
042900030129     c                   eval      $PFranco = *in30
043000030129      *
043100030129      * Controllo per Tipo Bancale:
043200030129     c                   movel     AR5uni        dAR5ban
043300030129     c                   clear                   Wtba
043400030129      *  1� bancale non ritirato
043500030129if  1c                   if        �AR5rb1 > *zeros
043600030129     c                   eval      Wtba = 1
043700030129     c                   exsr      Wrt_WFMGB
043800030129e   1c                   endif
043900030129      *  2� bancale non ritirato
044000030129if  1c                   if        �AR5rb2 > *zeros
044100030129     c                   eval      Wtba = 2
044200030129     c                   exsr      Wrt_WFMGB
044300030129e   1c                   endif
044400030129      *
044500030129     c                   ENDSR
044600030129      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -**
044700030129     c     Wrt_WFMGB     BEGSR
044800030129      *
044900030129     c                   clear                   WFMGB000
045000030129      *
045100030129      * Imposto i campi chiave
045200030129     c                   eval      WFMdat = Woggi
045300030129     c                   eval      WFMora = Wora
045400030129     c                   eval      WFMute = KNMUS
045500030129     c                   eval      WFMpou = SIMpou
045600030129     c                   eval      WFMpgm = SDSpgm
045700030214     c                   eval      WFMcau = 'MRDE'
045800030129     c                   eval      WFMsog = *zeros
045900030129sel 1c                   select
046000030129w   1c                   when      TASccm <> *zeros
046100030129     c                   move      TASccm        WFMsog
046200030129w   1c                   when      $PFranco = *on
046300030129     c                   move      TASksc        WFMsog
046400030129x   1c                   other
046500030129     c                   move      TASlnp        Wa03
046600030129     c                   eval      WFMsog = '0' + Wa03 + '8888'
046700030129e   1c                   endsl
046800030129if  1c                   If        Wtba = 1
046900030129     c                   movel(p)  �AR5tba       WFMcod
047000030129x   1c                   Else
047100030129     c                   movel(p)  �AR5tb2       WFMcod
047200030129e   1c                   Endif
047300030129      *
047400030130      * Imposto i campi da registrare
047500030130      * - quantit�
047600030129if  1c                   If        Wtba = 1
047700030130     c                   eval      WFMqta = �AR5rb1
047800030129x   1c                   else
047900030130     c                   eval      WFMqta = �AR5rb2
048000030129e   1c                   Endif
048100030130      * - riferimento documento movim.
048200030130     c                   eval      WFMrdo = dsRIFDOC
048300030130      * - data        documento movim.
048400030130     c                   eval      WFMddo = (TASaas * 10000) + TASmgs
048500030129      *
048600030130      * Aggiungo record
048700030129     c                   WRITE     WFMGB000
048800030129     c                   add       1             $RecWFM
048900030129      *
049000030129     c                   ENDSR
049100030120
049200030120      **-------------------------------------------------------------**
049300030120      ** Aggiorna l'archivio di controllo delle ricezioni            **
049400030120      **-------------------------------------------------------------**
049500030120     c     AggCMR        BEGSR
049600030120      *
049700030120     c     LEGfil        chain     CMRIC000
049800030120if  1c                   if        %found(CMRIC01L)                             *esistente
049900030120     c                   z-add     $Rec          RICnr1                         *n� record ricevuti
050000030120     c                   movel     '1'           RICfl2                         *archivio aggiornato
050100030120     c                   update    CMRIC000
050200030120     c                   move      RICprr        LEGfil
050300030120e   1c                   endif
050400030120      *
050500030120     c                   ENDSR
050600030120
050700030120      **-------------------------------------------------------------**
050800030120      ** Disalloca il membro di ricezione                            **
050900030120      **-------------------------------------------------------------**
051000030120     c     DisMBR        BEGSR
051100030120      *
051200030120      * cancella la sovrascrittura del membro di ricezione   e
051300030129      * disalloca il membro di ricezione - file FIAR500R
051400030129     c                   eval      UL25pf = 'FIAR500R  '
051500030120     c                   eval      UL25tc = '9'
051600030120     c                   exsr      CallTRUL25
051700030129      *
051800030129      * cancella la sovrascrittura del membro di ricezione   e
051900030129      * disalloca il membro di ricezione - file WFMGB01R
052000030129     c                   eval      UL25pf = 'WFMGB01R  '
052100030129     c                   eval      UL25tc = '9'
052200030129     c                   exsr      CallTRUL25
052300030120      *
052400030120     c                   ENDSR
052500030129
052600030129      **-------------------------------------------------------------**
052700030129      ** Richiamo al pgm. TISIG6R (azione batch SIG6)                **
052800030129      **-------------------------------------------------------------**
052900030129     c     CallTISIG6    BEGSR
053000030129      *
053100030129      * salvo i parametri passati
053200030129     c                   movel     KPJBA         SAVjba
053300030129      * imposto i parametri per il programma TISIG6R
053400030129      *   (ricezione movimenti gabbie/bancali)
053500030129     c                   eval      LEGfil = 'WFMGB00R  '
053600030129      * e lo richiamo
053700030129if  1c                   if        KNMUS = *all'1'
053800030129     c                   call      'TISIG6R'
053900030129     c                   parm                    KPJBA
054000030129x   1c                   else
054100030129     c                   eval      KCOAZ = 'SIG6'
054200030129     c                   call      'BCH10'
054300030129     c                   parm                    KPJBA
054400030129e   1c                   endif
054500030129      * quindi reimposto i parametri "originali"
054600030129     c                   movel     SAVjba        KPJBA
054700030129      *
054800030129     c                   ENDSR
