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
002000030129      *
002100030120     fFIAR500R  Uf   e             disk    usropn
002200030120     f                                     rename(FIAR5000:FIAR50R)
002300030120     f                                     prefix(R_)
002400040531     fFIAR531C  Uf a e           k disk
002500030120      *---------------------------------------------------------------*
002600030120
002700030120      * Costanti :
002800030120      *
002900030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003000030120      * Schiere :
003100030129      *
003200030129     d AR5n            s                   like(AR5trd)  dim(50)
003300030120      *
003400030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003500030120      * Strutture Dati :
003600030129      *
003700030129     d Status         sds           333
003800030129     d  SDSpgm           *proc
003900030120      *
004000030120     d KPJBA         e ds                                                       *architettura
004100030120     d  LEGfil               483    492                                          -libreria file
004200030120     d  MBRfil               493    502                                          -membro file
004300030129      *
004400030129     d UT§DSE0F      e ds
004500030129     d CNCR80        e ds
004600030120      *
004700150212     d TISIG0DS      e ds
004800150212      *
004900030120     d dsFIAR5R      e ds                  extname(FIAR500R) inz
005000030120     d                                     prefix(R_)
005100030120     d dsFIAR5F      e ds                  extname(FIAR500F) inz
005200030129      *
005300030129     d dsTB          e ds                  inz
005400030129     d dAR5          e ds                  inz
005500120405     d dAR5gen       e ds                  inz
005600030120      *
005700030120     d WLBDA8          ds                  inz                                  *ds "XSRDA8"
005800030120     d  G08dat                        8  0 inz                                   -data g/m/aa
005900030120     d  G08inv                        8  0 inz                                   -data aa/m/g
006000030120     d  G08err                        1    inz                                   -errore
006100030120     d  G08tgi                        5  0 inz                                   -n°giorni data
006200030130      *
006300030130     d dsRIFDOC        ds            14    inz
006400030130     d  TASlnp
006500030130     d  Wsep1                         1    inz('-')
006600030130     d  TASnrs
006700030130     d  Wsep2                         1    inz('-')
006800030130     d  TASnsp
006900030120      *
007000030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
007100030120      * Variabili
007200030120      *
007300030120      * - Campi di comodo
007400030801     d DataMax         s              8  0 Inz
007500030129     d SAVjba          s                   like(KPJBA) inz
007600030129     d Wtba            s              1  0 inz
007700030129     d Wa03            s              3    inz                                  *alfanum. 3
007800030801     d Wiso            s               d   datfmt(*iso)
007900030120     d Wn14            s             14  0                                      *numerico 14
008000030120     d Wn08            s              8  0                                      *numerico 8
008100030801     d Wnotitas        s              1
008200030120     d Woggi           s              8  0                                      *d.corrente aa/m/g
008300030129     d Wora            s              6  0                                      *h.corrente h:m:s
008400030129      * - Indici di schiera
008500030129     d xx              s              3  0 inz
008600030129      * - Contatori record estratti
008700030120     d $Rec            s              7  0 inz                                  *rec ricevuti
008800030129     d $RecWFM         s              7  0 inz
008900030120
009000030120      *---------------------------------------------------------------*
009100030120
009200030120     iTITAS000      20
009300030120     iTITAS010      21
009400030120     iTITASP00      22
009500030120      *
009600030120     iFIAR5000      50
009700030120     iFIAR5P00      51
009800030120
009900030120      *---------------------------------------------------------------*
010000030120
010100030120      * Verifica l'esistenza del membro di ricezione
010200030120     c                   exsr      EsiMBR
010300030120     c   97              goto      Fine
010400030120      *
010500030120      * Operazioni di ricezione
010600030120     c                   exsr      RicMBR
010700030120      *
010800030120      * Disalloca il membro di ricezione
010900030120     c                   exsr      DisMBR
011000030120      * Fine
011100030120     c     Fine          tag
011200030120     c                   eval      *inLR = *on
011300030120
011400030120      **-------------------------------------------------------------**
011500030120      ** Operazioni iniziali                                         **
011600030120      **-------------------------------------------------------------**
011700030120     c     *InzSR        BEGSR
011800030120      *
011900030120     c     *Entry        Plist
012000030120     c                   parm                    KPJBA
012100030120      *
012200030120      * Chiavi di lettura:
012300040531      * - FIAR531C > completa (8 campi chiave)
012400030120     c     K08AR501      klist
012500030120     c                   kfld                    r_AR5aas
012600030120     c                   kfld                    r_AR5lnp
012700030120     c                   kfld                    r_AR5nrs
012800030120     c                   kfld                    r_AR5nsp
012900030120     c                   kfld                    r_AR5trd
013000040524     c                   kfld                    r_AR5prg
013100030120     c                   kfld                    r_AR5dac
013200030120     c                   kfld                    r_AR5orc
013300030120      * - TITAS30C > INcompleta (4 campi chiave)
013400030120     c     K04TAS30      klist
013500030120     c                   kfld                    r_AR5aas
013600030120     c                   kfld                    r_AR5lnp
013700030120     c                   kfld                    r_AR5nrs
013800030120     c                   kfld                    r_AR5nsp
013900030129      *
014000030129      * reperisco i dati aziendali
014100030129     c                   call      'X§PARUT'
014200030129     c                   parm                    UT§DSE0F
014300030129     c                   movel     Rec80         CNCR80
014400030120      *
014500030120      * calcola la data corrente
014600030120     c                   time                    Wn14                           *ora(6) + data(8)
014700030129     c                   movel     Wn14          Wora                           *ora(6)
014800030120     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
014900030120     c                   z-add     Wn08          G08dat
015000030120     c                   clear                   G08inv
015100030120     c                   movel     *off          G08err
015200030120     c                   call      'XSRDA8'
015300030120     c                   parm                    WLBDA8
015400030120     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
015500030801      * Data corrente iso
015600030801     c     *iso          Movel     Woggi         Wiso
015700030801      * Data max per cancellazione record già ricevuti
015800030801     c                   Subdur    10:*d         Wiso
015900030801     c                   Move      Wiso          DataMax
016000030120      *
016100030120      * se il membro da ricevere non è stato dichiarato,
016200030120      *   assume il primo del file
016300030120if  1c                   if        MBRfil = *blanks
016400030120     c                   eval      MBRfil = '*FIRST'
016500030120e   1c                   endif
016600030129      *
016700030129      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
016800030129     c                   clear                   xx
016900030129     c                   clear                   AR5n
017000030129     c                   eval      TBEcod = 'AR5'
017100030129     c     TBEcod        setll     TNTBE000
017200030129     c     TBEcod        reade     TNTBE000
017300030129do  1c                   DOW       Not %Eof(TNTBE01L)
017400030129if  2c                   IF        TBEatb = *blanks
017500030129     c                   eval      dAR5 = TBEuni
017600030129if  3c                   If        §AR5trmi <> 'S'
017700030129     c                   add       1             xx
017800030129     c                   movel     TBEke1        AR5n(xx)
017900030129e   3c                   Endif
018000030129e   2c                   ENDIF
018100030129     c     TBEcod        reade     TNTBE000
018200030129e   1c                   ENDDO
018300030120      *
018400030120     c                   ENDSR
018500030120      **-------------------------------------------------------------**
018600030120      ** Verifica l'esistenza del membro di ricezione                **
018700030120      **-------------------------------------------------------------**
018800030120     c     EsiMBR        BEGSR
018900030120      *
019000030129      * File FIAR500R:
019100030129     c                   eval      UL25pf = 'FIAR500R  '
019200030129      *
019300030120      * controlla esistenza membro di ricezione,
019400030120      *   se non esiste (*in97 = *on) chiude il programma
019500030120     c                   eval      UL25tc = '2'
019600030120     c                   exsr      CallTRUL25
019700030120     c   97              goto      F_EsiMBR
019800030120      *
019900030120      * alloca il membro di ricezione,
020000030120      *   se non riesce (*in97 = *on) chiude il programma
020100030120     c                   eval      UL25tc = '4'
020200030120     c                   exsr      CallTRUL25
020300030120     c   97              goto      F_EsiMBR
020400030120      *
020500030120      * esegue sovrapposizione membro di ricezione,
020600030120      *   se non riesce (*in97 = *on) chiude il programma
020700030120     c                   eval      UL25tc = '5'
020800030120     c                   exsr      CallTRUL25
020900030120     c   97              goto      F_EsiMBR
021000030120      *
021100030120     c     F_EsiMBR      ENDSR
021200030120
021300030120      **-------------------------------------------------------------**
021400030120      ** Richiamo cl TRUL25C                                         **
021500030120      **-------------------------------------------------------------**
021600030120     c     CallTRUL25    BEGSR
021700030120      *
021800030120     c                   call      'TRUL25C'                            97
021900030120     c                   parm                    UL25tc            1
022000030129     c                   parm                    UL25pf           10
022100030120     c                   parm      *blanks       UL25li           10
022200030120     c                   parm      MBRfil        UL25mb           10
022300030120     c                   parm      *blanks       UL25tx           10
022400030120     c                   parm      *blanks       UL25fr            1
022500030120     c                   parm      *blanks       UL25os            1
022600030120     c                   parm      *zeros        UL25es            2
022700030120      *
022800030120     c  N97UL25es        comp      *zeros                             9797
022900030120      *
023000030120     c                   ENDSR
023100030120      **-------------------------------------------------------------**
023200030120      ** Operazioni di ricezione                                     **
023300030120      **-------------------------------------------------------------**
023400030120     c     RicMBR        BEGSR
023500030120      *
023600030120      * apre il file di ricezione
023700030120     c                   open      FIAR500R
023800030120      *
023900030120      * legge il file di ricezione
024000030120     c                   read      FIAR50R
024100030120do  1c                   dow       Not %eof(FIAR500R)
024200030120      *
024300030120      * scrive il record fisico
024400030120     c                   exsr      ScrPFm
024500030120      * cancella il record di ricezione
024600030801      * sempre se trovato Titas
024700030801If  1c                   If        Wnotitas = *Blanks
024800030120     c                   delete    FIAR50R
024900030801   x1c                   Else
025000030801      * se sono passati tot. giorni dalla creazione del rcd se non trovato Titas
025100030910If  2c                   If        r_Ar5Dac <= DataMax
025200030801     c                   Delete    FIAR50R
025300030801    2c                   EndIf
025400030801    1c                   EndIf
025500030120      *
025600030120     c                   read      FIAR50R
025700030120e   1c                   enddo                                                  *fine file ricezione
025800030120      *
025900030120      * chiude il file di ricezione
026000030120     c                   close     FIAR500R
026100030120      *
026200030120     c                   ENDSR
026300030120      **-------------------------------------------------------------**
026400030120      ** Scrive il record fisico                                     **
026500030120      **-------------------------------------------------------------**
026600030120     c     ScrPFm        BEGSR
026700030120      *
026800030801     c                   Clear                   Wnotitas
026900030120      * controlla se esiste la bolla:
027000030120      * carico estensioni solo se esiste la testata
027100030120     c                   movea     '000'         *in(20)
027200030120     c     K04TAS30      chain     TITAS30C
027300030120if  1c                   if        %found(TITAS30C)
027400030120      *
027500030120      * aggancia il record da aggiornare
027600030120     c                   movea     '00'          *in(50)
027700040531     c     K08AR501      chain     FIAR531C
027800120405     c                   clear                   dar5gen
027900120405if  2c                   if        %found(FIAR531C)  and ar5trd='GEN'
028000120405     c                   eval      dar5gen=ar5uni
028100120405     c                   endif
028200030120      *
028300030120      * imposta i campi del file fisico con quelli del record ricevuto
028400030120     c                   movel     dsFIAR5r      dsFIAR5f                       *formato record
028500120405     c
028600120405if  2c                   if        %found(FIAR531C)  and ar5trd='GEN'
028700120405     c
028800120405     c* risposto i campi solo SEDE per evitare che si abblankino
028900120405     c                   eval      %subst(ar5uni:107:8)=§ar5drdt
029000120405     c                   eval      %subst(ar5uni:115:1)=§AR5FSTD
029100120405     c                   eval      %subst(ar5uni:167:8)=§AR5MTVDT
029200120405     c                   eval      %subst(ar5uni:175:8)=§AR5DIMTV
029300120405     c                   endif
029400030120      *
029500030130      * - fleggo come ricevuto in filiale
029600030130     C                   eval      AR5ft1 = 'R'
029700030130     C                   eval      AR5dt1 = Woggi
029800030129      * - fleggo come ricevuto in sede
029900030129     c                   eval      AR5ft2 = 'R'
030000030129     c                   eval      AR5dt2 = Woggi
030100030129      * - verifico se per il tipo record di FIAR5 è previsto il
030200030129      *   ritorno info al cliente: se non è previsto imposto
030300030129      *   il record come già trasmesso (cioè da non trasmettere).
030400030129     c                   eval      *in30  =  *off
030500030129     c     AR5trd        lookup    AR5n                                   30
030600030129if  2c                   IF        *in30
030700030129     c                   eval      AR5ft3 = 'T'
030800030129     c                   eval      AR5dt3 = *all'9'
030900030129x   2c                   ELSE
031000030129     c                   clear                   AR5ft3
031100030129     c                   clear                   AR5dt3
031200030129e   2c                   ENDIF
031300030120      *
031400040531if  2c                   if        Not %found(FIAR531C)
031500040308      * se record 'GEN' a ar5uni è vuoto non devo scriverlo
031600040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
031700040308    3c                   Else
031800030120      * inesistente, inserimento
031900030120     c   20              write     FIAR5000
032000030120     c   21              write     FIAR5000
032100030120     c   22              write     FIAR5P00
032200040308    3c                   EndIf
032300030120x   2c                   else
032400040308      * se record 'GEN' a ar5uni è vuoto devo deletare il record
032500040308if  3c                   If        Ar5Trd = 'GEN' and Ar5Uni = *Blanks
032600040308     c   50              delete    FIAR5000
032700040308     c   51              delete    FIAR5P00
032800040308    3c                   Else
032900030120      * esistente, aggiornamento
033000030120     c   50              update    FIAR5000
033100030120     c   51              update    FIAR5P00
033200040308    3c                   EndIf
033300030120e   2c                   endif
033400030129      *
033500030120      *
033600030801   x2c                   Else
033700030801     c                   Eval      wnotitas = '1'
033800030120e   1c                   endif
033900030120      *
034000030120      * conteggio dei record ricevuti
034100030120      *   (anche se non esiste più la testata, quindi come se non fosse
034200030120      *   stato "ricevuto")
034300030120     c                   add       1             $Rec                           *n° record ricevuti
034400030120      *
034500030120     c                   ENDSR
034600030129
034700030120      **-------------------------------------------------------------**
034800030120      ** Disalloca il membro di ricezione                            **
034900030120      **-------------------------------------------------------------**
035000030120     c     DisMBR        BEGSR
035100030120      *
035200030120      * cancella la sovrascrittura del membro di ricezione   e
035300030129      * disalloca il membro di ricezione - file FIAR500R
035400030129     c                   eval      UL25pf = 'FIAR500R  '
035500030120     c                   eval      UL25tc = '9'
035600030120     c                   exsr      CallTRUL25
035700030120      *
035800030120     c                   ENDSR
035900150212     C*--------------------------------------------------------------------------------------------*
