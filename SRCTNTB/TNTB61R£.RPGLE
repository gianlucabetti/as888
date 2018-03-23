000100071031     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200071031      *--------------------------------------------------------------------------------------------*
000300071112      *     Personalizzazioni ambiti giri di ritiro                                                *
000400071031      *--------------------------------------------------------------------------------------------*
000500071031      * Data base                                                                                  *
000600071031      *--------------------------------------------------------------------------------------------*
000700071031     fazorg01l  if   e           k disk
000800071031     ftntbe01l  uf a e           k disk
000900071031     ftntb61d   cf   e             workstn
001000071031      *--------------------------------------------------------------------------------------------*
001100071031      * Data structure                                                                             *
001200071031      *--------------------------------------------------------------------------------------------*
001300071031     d msg             s             78    dim(6) ctdata perrcd(1)
001400071112
001500071112     d tipvid          s              1
001600071112     d $apl            s              2
001700071112     d $fine           s              1
001800071112     d $lin            s              1
001900071112     d $sif            s             10
002000071031
002100071112     d azuteds       e ds                  inz extname(AZUTE00F)
002200071112     d dagr          e ds
002300071112     d ddatiute      e ds                  inz
002400071112     d fnlv50ds      e ds
002500071112     d kpjba         e ds
002600071031     d og147         e ds
002700071031     d tibs34ds      e ds                  inz
002800071031      *--------------------------------------------------------------------------------------------*
002900071031      * Main lines
003000071031      *--------------------------------------------------------------------------------------------*
003100071031
003200071031     c                   exsr      rutinz
003300071031
003400071031     c     $fine         doueq     'S'
003500071031     c     tipvid        caseq     '1'           gesvd1
003600071031     c     tipvid        caseq     '2'           gesvd2
003700071031     c                   endcs
003800071031     c                   enddo
003900071031
004000071031     c                   eval      *inlr = *on
004100071031
004200071031      *--------------------------------------------------------------------------------------------*
004300071031      * GesVd1 - Gestione videata immissione nuova tabella -
004400071031      *--------------------------------------------------------------------------------------------*
004500071031     c     gesvd1        begsr
004600071031
004700020208      * Inizializzazione videata
004800070126     c                   clear                   v1cfil
004900070126     c                   clear                   v1dfil
005000071031
005100071031     c     for01         tag
005200071031     c                   exfmt     tb61v1
005300071031     c                   clear                   v1cmsg
005400071112     c                   setoff                                       28
005500071031
005600071031      * F3 -  fine
005700071031     c                   if        *inkc
005800071031     c                   exsr      f03ges
005900071112     c                   leavesr
006000071031     c                   endif
006100071031
006200020208      * Controlli e decodifiche
006300071031     c                   exsr      ctrv1
006400071031
006500071031      * Errori
006600071112     c   28              goto      for01
006700071031
006800071112     c                   eval      tipvid = '2'
006900071031
007000071112     c                   endsr
007100071031
007200020208      *--------------------------------------------------------------------------------------------*
007300020208      * Controllo e decodifica prima videata
007400020208      *--------------------------------------------------------------------------------------------*
007500071031     c     ctrv1         begsr
007600071031
007700071112     c                   setoff                                       50
007800071031
007900071031      * Filiale
008000071031     c                   exsr      ctrfil
008100071112     c   28              leavesr
008200071031
008300071112      * Verifico se abilitato a GEO ORM se no EDP
008400070126     c                   if        not *in01
008500071031     c                   eval      og147 = orgde7
008600071031     c                   if        §ogcgio = *blanks
008700071112     c                   seton                                        2850
008800071031     c                   eval      v1cmsg = msg(2)
008900071112     c                   leavesr
009000071031     c                   endif
009100071031     c                   endif
009200071031
009300071112     c                   endsr
009400071031
009500071031      *--------------------------------------------------------------------------------------------*
009600071031      *  Controllo Filiale
009700071031      *--------------------------------------------------------------------------------------------*
009800071031     c     ctrfil        begsr
009900071031
010000071031     c     v1cfil        chain     azorg01l
010100071112     c                   if        not %found(azorg01l) or
010200071112     c                             orgfva <> *blanks or
010300071112     c                             (orgfag <> 'A' and orgfag <> 'F')
010400071112     c                   seton                                        2850
010500071031     c                   eval      v1cmsg = msg(1)
010600071112     c                   leavesr
010700071031     c                   endif
010800071031
010900071031     c                   eval      v1dfil = orgdes
011000071031
011100071031      * Se non sono EDP deve essere p.o. della £1
011200070126     c                   if        not *in01
011300071031     c                   clear                   fnlv50ds
011400071031     c                   eval      d50pru = knmus
011500071031     c                   eval      d50fgs = v1cfil
011600071031     c                   call      'FNLV50R'
011700071031     c                   parm                    fnlv50ds
011800071031     c                   if        d50err <> *blanks
011900071031     c                   eval      v1cmsg = d50msg
012000071112     c                   seton                                        2850
012100071112     c                   leavesr
012200071031     c                   endif
012300071031     c                   endif
012400071031
012500071112     c                   endsr
012600071031
012700020208      *--------------------------------------------------------------------------------------------*
012800071031      * GesVd2 - Gestione seconda videata
012900020208      *--------------------------------------------------------------------------------------------*
013000071031     c     gesvd2        begsr
013100071031
013200071031     c                   exsr      carvd2
013300071031
013400070126     c     for02         tag
013500071031     c                   exfmt     tb61v2
013600071031
013700070126     c                   clear                   v1cmsg
013800071112     c                   setoff                                       28
013900071031
014000071031      * F3 - fine lavoro
014100070126     c                   if        *inkc
014200071031     c                   exsr      f03ges
014300071112     c                   leavesr
014400070126     c                   endif
014500071031
014600071031      * F12 - ritorno
014700070126     c                   if        *inkl
014800071031     c                   eval      tipvid = '1'
014900071112     c                   leavesr
015000070126     c                   endif
015100071031
015200071031      * F16 - annullamento
015300070126     c                   if        *inkq
015400071112      * errore se immissione record
015500070126     c                   if        not %found(tntbe01l)
015600071112     c                   eval      v1cmsg = msg(1)
015700071112     c                   seton                                        2840
015800071112     c                   leavesr
015900071031     c                   endif
016000070126     c                   delete    tntbe000
016100071031     c                   eval      tipvid = '1'
016200071112     c                   leavesr
016300070126     c                   endif
016400071031
016500071031     c                   exsr      ctrv2
016600071031
016700071112     c                   if        *in28  or not *inkf
016800070126     c                   goto      for02
016900070126     c                   endif
017000071031
017100071031      * F6 - Aggiorna
017200070126     c                   if        *inkf
017300071031     c                   exsr      aggiorna
017400070126     c                   endif
017500071031
017600071031     c                   eval      tipvid = '1'
017700070129     c                   clear                   v1cfil
017800070129     c                   clear                   v1dfil
017900071031
018000071112     c                   endsr
018100071031
018200020208      *--------------------------------------------------------------------------------------------*
018300020208      * CarVd2 - Carica primo video
018400020208      *--------------------------------------------------------------------------------------------*
018500071031     c     carvd2        begsr
018600071031
018700020211      * Leggo la tabella
018800071031     c                   movel(p)  v1cfil        tbeke1
018900071031     c                   movel     *blanks       tbeke2
019000071031     c     ktbe          chain     tntbe01l
019100071031
019200071031      * Se trovata carico i dati
019300070126     c                   if        %found(tntbe01l)
019400071031     c                   movel     tbeuni        dagr
019500070126     c                   eval      v1cmod='VARIAZIONE '
019600070126     c                   else
019700071031     c                   clear                   dagr
019800071031     c                   eval      v1cmod = 'INSERIMENTO'
019900070126     c                   endif
020000071031
020100071031     c                   eval      v1cpkgo = §agrpkgo
020200071031     c                   eval      v1cvlmo = §agrvlmo
020300071031     c                   eval      v1cbnco = §agrbnco
020400071031     c                   eval      v1cpkgs = §agrpkgs
020500071031     c                   eval      v1cvlms = §agrvlms
020600071031     c                   eval      v1cbncs = §agrbncs
020700071031
020800071031
020900071031     c                   endsr
021000071031
021100020208      *--------------------------------------------------------------------------------------------*
021200020215      * Controllo seconda videata
021300020208      *--------------------------------------------------------------------------------------------*
021400071031     c     ctrv2         begsr
021500071031
021600071112     c                   setoff                                       414243
021700071112     c                   setoff                                       444546
021800071112
021900071112     c                   select
022000071112      * i limiti sotto posso lasciarli a zero tutti
022100071112     c                   when      v1cpkgs = 0 and v1cvlms = 0 and v1cbncs = 0
022200071112      * i limiti oltre devono esserci tutti
022300071112     c                   when      v1cpkgo > 0 and v1cvlmo > 0 and v1cbnco > 0
022400071112     c                   other
022500071031
022600071031     c     v1cpkgo       comp      0                                      43
022700071031     c     v1cvlmo       comp      0                                      44
022800071031     c     v1cbnco       comp      0                                      46
022900071112     c                   seton                                          28
023000071031     c                   eval      v1cmsg = msg(3)
023100071112     c                   leavesr
023200071112     c                   endsl
023300071112
023400071112      * il limite sotto non deve essere >= al limite oltre
023500071112     c                   if        v1cpkgs >= v1cpkgo
023600071112     c                   seton                                          284190
023700071112     c                   eval      v1cmsg = msg(4)
023800071112     c                   leavesr
023900071112     c                   endif
024000071112     c                   if        v1cvlms >= v1cvlmo
024100071112     c                   seton                                          284290
024200071112     c                   eval      v1cmsg = msg(4)
024300071112     c                   leavesr
024400071112     c                   endif
024500071112     c                   if        v1cbncs >= v1cbnco
024600071112     c                   seton                                          284590
024700071112     c                   eval      v1cmsg = msg(4)
024800071112     c                   leavesr
024900071112     c                   endif
025000071031
025100071112     c                   endsr
025200071031
025300020208      *--------------------------------------------------------------------------------------------*
025400070126      * Aggiornamenti
025500020208      *--------------------------------------------------------------------------------------------*
025600071031     c     aggiorna      begsr
025700071031
025800071031     c                   clear                   tntbe000
025900071031     c                   move      ' '           tbeatb
026000071031     c                   movel     $sif          tbesif
026100071031     c                   movel     $lin          tbelin
026200071031     c                   movel     $apl          tbeapl
026300071031     c                   move      'AGR'         tbecod
026400071031     c                   move      *blanks       tbeke1
026500071031     c                   move      *blanks       tbeke2
026600070126     c                   movel     v1cfil        tbeke1
026700071031      * campi della DS
026800071031     c                   movel     v1cpkgs       §agrpkgs
026900071031     c                   movel     v1cvlms       §agrvlms
027000071031     c                   movel     v1cbncs       §agrbncs
027100071031     c                   movel     v1cpkgo       §agrpkgo
027200071031     c                   movel     v1cvlmo       §agrvlmo
027300071031     c                   movel     v1cbnco       §agrbnco
027400071031
027500071031     c                   movel(p)  dagr          tbeuni
027600071031
027700071031     c                   move      ' '           tbeftt
027800071031     c                   z-add     *zeros        tbeflt
027900071031     c                   move      ' '           tbeftr
028000071031     c                   move      *zeros        tbedtr
028100071031
028200070126     c                   if        %found(tntbe01l)
028300070126     c                   update    tntbe000
028400070126     c                   else
028500071031     c                   write     Tntbe000
028600070126     c                   endif
028700071031
028800071031     c                   endsr
028900071031
029000020208      *--------------------------------------------------------------------------------------------*
029100020208      * F03ges - Tasto funzionale f03 -> Fine programma
029200020208      *--------------------------------------------------------------------------------------------*
029300071031     c     f03ges        begsr
029400071031
029500071112     c                   eval      $fine = 'S'
029600071031
029700071031     c                   endsr
029800071031
029900071031      *--------------------------------------------------------------------------------------------*
030000071031      * RutInz - operazioni iniziali
030100071031      *--------------------------------------------------------------------------------------------*
030200071031     c     rutinz        begsr
030300071031
030400071031     c     *entry        plist
030500071031     c                   parm                    kpjba
030600071031
030700071031      * Reperisco dati utente
030800071031     c     *dtaara       define    §azute        azuteds
030900071031     c     *dtaara       define    §datiute      ddatiute
031000071031
031100071031     c                   clear                   azuteds
031200071031     c                   clear                   ddatiute
031300071031     c                   clear                   tibs34ds
031400071031     c                   in(e)     *dtaara
031500071031     c                   if        %error or rsut = *blanks
031600071031     c                   call      'TIBS34R'
031700071031     c                   parm                    tibs34ds
031800071031     c                   in        *dtaara
031900071031     c                   endif
032000071031
032100071112     c                   eval      tipvid = '1'
032200071031
032300020211      * Reperisco testata tabella
032400071031     c                   clear                   tbecod
032500071031     c                   eval      tbeke1 = *all'0'
032600071031     c                   move      'AGR'         tbeke1
032700071031     c     k01tbe2       chain     tntbe01l
032800071031     c                   if        not %found(tntbe01l)
032900071031     c                   eval      $fine = '1'
033000071031     c                   endif
033100071031     c                   if        %found(tntbe01l)
033200071112     c                   movel     tbesif        $sif
033300071112     c                   movel     tbeapl        $apl
033400071112     c                   movel     tbelin        $lin
033500071031     c                   move      'AGR'         tbecod
033600071031     c                   endif
033700071031
033800071031      * EDP tutto è possibile
033900071031     c                   eval      *in01 = (%subst(knmus:1:3) = 'EDP')
034000071031     c                   movel     rsut          v1crag
034100071031     c                   movel     knmus         v1cute
034200071031     c                   movel     knsif         v1csif
034300071031
034400071031     c     k01tbe2       klist
034500071031     c                   kfld                    tbecod
034600071031     c                   kfld                    tbeke1
034700071031
034800071031     c     ktbe          klist
034900071031     c                   kfld                    tbecod
035000071031     c                   kfld                    tbeke1
035100071031     c                   kfld                    tbeKe2
035200071031     c                   kfld                    $lin
035300071031     c                   kfld                    $sif
035400071031
035500071031     c                   endsr
035600071031
035700020212** Msg
035800071031Filiale inesistente o errata
035900071031Filiale non abilitato alla Procedura GEO ORM
036000071031Se immesso un limite immettere anche gli altri due
036100071112Limite sotto >= del limite oltre
