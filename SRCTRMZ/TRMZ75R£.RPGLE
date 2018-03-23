000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200071207     faitra01l  if   e           k disk
000300090415     Faitrs01l  IF   E           K DISK
000400090127     faiatm01l  if   e           k disk
000500071207     ffiapd01l  if   e           k disk
000600071207     fanrco98j  if   e           k disk
000700071212     faiass00f  uf a e           k disk
000800080827      *------------------------------------------------------
000900000607     D kpjba         e ds
001000080827      *
001100090415     D trmz70s1ds    E DS                  prefix(S1_)
001200000607      *--------------------------------------------------------------------
001300090127     c                   exsr      autisticitta
001400090127     c
001500090127     c                   exsr      autistitraini
001600090127
001700090127     c                   seton                                        lr
001800090127      * ?_____________________________________________________________________
001900090127     c* ?  autisticitta  elabora archivio autisti di città
002000090127      * ?_____________________________________________________________________
002100090127     c     autisticitta  begsr
002200090127      * lettura
002300071207     c     *loval        setll     aitra01l
002400080827      *
002500000607     c                   do        *hival
002600071207     c                   read      aitra01l
002700080827      *
002800071207     c                   if        %eof(aitra01l)
002900000607     c                   leave
003000000607     c                   end
003100090415      *
003200090415      *   Aggancia la testata della Società
003300090415     c     tranrc        chain     AitrS01l
003400090415     c                   if        not %found(AitrS01l)
003500090415     c                   eval      assrags = 'Manca Anagr.società (TRS) *******'
003600090415     c                   eval      assind  = %editc(trafil:'Z') + ' ' + trasoc
003700090415     c                   eval      assind  = %trim(assind) + '/' + traAUT
003800090415     c                   end
003900090415      *
004000080828      * Legge e non considera il rekord se
004100080828      *        disaccreditato o
004200080828      *        autista è di riserva o
004300080828      *        corrispondente
004400080401     c                   if           tradfi > 0
004500080828     c                             or traris <> *blank
004600090415     c                             or trScor <> *blank
004601091006     c                             or tracor <> *blank
004700071207     c                   iter
004800071207     c                   end
004900080401      *
005000071212     c                   clear                   aiass000
005100080828      *
005200080828     c                   eval      assrags = 'Manca P.IVA *********************'
005300071212     c                   eval      assind  = %editc(trafil:'Z') + ' ' + trasoc
005400080827     c                   eval      assind  = %trim(assind) + '/' + traAUT
005500080827     c                   clear                   ksc8
005600080827      **
005700080827      **  Se c'è l'autista
005800080828      **    cerco il fornitore/società nell'anagrafica degli AUT:
005900080827     c                   if        traKAU > 0
006000071207     c     kapd          chain     fiapd01l
006100071207     c                   if        %found(fiapd01l)
006200071212     c                   z-add     apdksc        assKSC
006300071212     c                   move      apdcsf        assSOC
006400080827     c                   eval       Cod_Societa = apdcsf
006500071207     c                   move      *zeros        ksc8
006600071207     c                   move      apdksc        ksc8
006700080827     c                   endif
006800080827     c                   endif
006900080827      **
007000080828      **  Se non c'è l'autista o
007100080828      **  se non è stato trovato il Fornitore
007200080828      **    vado a cercare con la P.IVA su PROJ:
007300080828     c                   If        assKSC = 0  and
007400090415     c                             trSIVA <>' '
007500080827     c                   exsr      Find_Fornitore
007600080827     c                   endif
007700080827      **
007800080828      ** Con il codice Fornitore ( Società )
007900080828      **  Reperisce i dati anagrafici del Fornitore da PROJ:
008000080828     c                   if        Ksc8 <> *blank
008100071207     c     krco          chain     anrco98j
008200071207     c                   if        %found(anrco98j)
008300071212     c                   movel     sogdes        assRAGS
008400071212     c                   movel     indindriz     assIND
008500071212     c                   movel     indlocalit    assLOC
008600071212     c                   movel     indcap        assCAP
008700071212     c                   movel     indprov       assPROV
008800071212     c                   movel     indstato      assstato
008900071212     c                   movel     sogpartiva    assPIVA
009000071207     c                   endif
009100071207     c                   endif
009200080827      **
009300071212     c                   movel     tratia        assMARCA
009400071212     c                   movel     tradass       assANIMM
009500071212     c                   movel     trataa        assTARGA
009600071212     c                   movel     trafil        assfgs
009700080331     c                   movel     traTIP        assTPA
009800080331     c                   movel     traFUR        assTIA
009900071207     c
010000071212     c                   write     aiass000
010100040409     c                   enddo
010200090415     c**
010300090127     c                   endsr
010400090127      * ?_____________________________________________________________________
010500090127     c* ?  autistitraini elabora archivio autisti di città
010600090127      * ?_____________________________________________________________________
010700090127     c     autistitraini begsr
010800090127     c     *loval        setll     aiatm01l
010900090127      *
011000090127     c                   do        *hival
011100090127     c                   read      aiatm01l
011200090127      *
011300090127     c                   if        %eof(aiatm01l)
011400090127     c                   leave
011500090127     c                   end
011600090127      * disaccreditati o annullati
011700090127     c                   if           atmdtfir > 0
011800090127     c                             or atmatb <> *blank
011900090127     c                   iter
012000090127     c                   end
012100090127      *
012200090128     c                   clear                   aiass000
012300090128     c                   move      *zeros        ksc8
012400090128     c                   move      atmksc        ksc8
012500090128     c                   move      atmsoc        Cod_Societa
012600090128      **
012700090128      ** Con il codice Fornitore ( Società )
012800090128      **  Reperisce i dati anagrafici del Fornitore da PROJ:
012900090128     c     krco          chain     anrco98j
013000090128     c                   if        %found(anrco98j)
013100090128     c                   movel     sogdes        assRAGS
013200090128     c                   movel     indindriz     assIND
013300090128     c                   movel     indlocalit    assLOC
013400090128     c                   movel     indcap        assCAP
013500090128     c                   movel     indprov       assPROV
013600090128     c                   movel     indstato      assstato
013700090128     c                   movel     sogpartiva    assPIVA
013800090128     c                   endif
013900090128      **
014000090128     c                   movel     atmtipo       assMARCA
014100090128     c                   movel     atmanno       assANIMM
014200090128     c                   movel     atmtarga      assTARGA
014300090128     c                   movel     atmksc        assksc
014400090128     c                   movel     atmsoc        asssoc
014500090128     c                   move      'T'           assTPA
014600090205     c                   z-add     046           assfgs
014700090128
014800090128     c                   write     aiass000
014900090128     c                   enddo
015000090128     c                   endsr
015100000607      *-----------------------------------------------
015200000607     c     *inzsr        begsr
015300000607      *-----------------------------------------------
015400000607     c
015500000607     c     *entry        plist
015600000607     c                   parm                    kpjba
015700071207     c     kapd          klist
015800071207     c                   kfld                    tip
015900071207     c                   kfld                    trakau
016000071207     c     krco          klist
016100080827     c*****              kfld                    apdcsf
016200080827     c                   kfld                    Cod_Societa       3
016300071207     c                   kfld                    natura
016400071207     c                   kfld                    ksc8              8
016500071207     c                   move      'F'           natura            1
016600071207     c                   move      'A'           tip               1
016700040409     c                   endsr
016800080827      *-----------------------------------------------
016900080828     c* Con Società / Unità / P.IVA cerca Fornitore su PROJ
017000080827      *-----------------------------------------------
017100080827     c     Find_Fornitorebegsr
017200080827      **
017300080827      **  Routine x Reperire dati Fornitore:
017400080827      **    La routine in base alla sottonatura può funzionare
017500080827      **     x F=Fornitore/C=Cliente
017600090415      **  Routine x Reperire dati Fornitore:
017700090415      **    La routine in base alla sottonatura può funzionare
017800090415      **     x F=Fornitore/C=Cliente
017900090415     C                   clear                   trmz70s1ds                     Input
018000090415     C                   movel(p)  trSIVA        s1_PartitaIVA                  Input
018100090415     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
018200090415     C                   movel(p)  trssocG       s1_Societa                     Input/Output
018300090415     C                   movel(p)  traFIL        s1_Unita                       Input/Output
018400090415     c                   call      'TRMZ70SR1'
018500090415     C                   PARM                    trmz70s1ds                     Input
018600090415      *
018700080828     c                   clear                   ksc8
018800080827      **
018900090415     C                   If        S1_Trovato = *On
019000080828     c                   move      *zeros        ksc8
019100090415     c                   move      S1_KeyKSC     ksc8
019200090415     C                   movel(p)  S1_Societa    Cod_Societa                    Input/Output
019300090415     c                   move(p)   S1_KeyKSC     assKSC
019400090415     c                   move      S1_Societa    assSOC
019500080827     c                   end
019600080827      **
019700080827     c                   endsr
019800080827      *-----------------------------------------------
