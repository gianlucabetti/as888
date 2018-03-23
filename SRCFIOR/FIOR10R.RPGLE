000100071220     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200071227
000300071220      *------------------------------------------------------------------------*
000400071220      *         STAMPA ANAGRAFICA CLIENTI RITIRO - filtro
000500071220      *------------------------------------------------------------------------*
000600071220
000700071220     fazorg01l  if   e           k disk
000800071220     ffior10d   cf   e             workstn
000900071220
001000071220      *------------------------------------------------------------------------*
001100071220      *  RIEPILOGO INDICATORI
001200071220      *------------------------------------------------------------------------*
001300071220      *  01 - Posiz. Cursore su Selez. per Codice
001400071227      *  02 - Posiz. Cursore su Filiale Giro di Ritiro
001500071227      *  03 - Posiz. Cursore su Codice Cliente Ritiro "da"
001600071220      *  04 - Posiz. Cursore su Ragione Sociale
001700071220      *  07 - Posiz. Cursore su P.O. Gestione/Appartenenza
001800071220      *  08 - Posiz. Cursore su Giro di Ritiro
001900071220      *  09 - Posiz. Cursore su Codice PdC
002000071227      *  10 - Posiz. Cursore su Codice Cliente Ritiro " a"
002100080108      *  15 - Filiale utente attiva con GEO ORM
002200071220      *  28 - Visualizzo msg Errore
002300071220
002400071220      *------------------------------------------------------------------------*
002500071220      *   V A R I A B I L I
002600071220      *------------------------------------------------------------------------*
002700071220
002800071220      *------------------------------------------------------------------------*
002900071220      *   S C H I E R E
003000071220      *------------------------------------------------------------------------*
003100071227     d msg             s             78    dim(10) ctdata perrcd(1)
003200071220
003300071220      *------------------------------------------------------------------------*
003400071220      *   D S   I N T E R N E / E S T E R N E
003500071220      *------------------------------------------------------------------------*
003600071220     d azuteds       e ds                  extname(azute00f)
003700071220     d ddatiute      e ds
003800071227     d ds_cnaco      e ds                  inz extname(cnaco00f)
003900071227     d ds_cnind      e ds                  inz extname(cnind00f)
004000071227     d ds_cnclp      e ds                  inz extname(cnclp00f)
004100071227     d ds_fncls      e ds                  inz extname(fncls00f)
004200071227     d fidg09ds      e ds
004300071227     d fior11ds      e ds
004400071220     d kpjba         e ds
004500080108     d og147         e ds
004600071220     d tibs34ds      e ds
004700071227     d tibs69ds      e ds                  inz
004800071220
004900071220     d                sds
005000071220     d  vidpgm                 1     10
005100071220
005200071220      *------------------------------------------------------------------------*
005300071220      *   C O S T A N T I
005400071220      *------------------------------------------------------------------------*
005500071220
005600071220      *------------------------------------------------------------------------*
005700071220
005800071220     c                   do        *hival
005900071220
006000071220      * emissione video
006100071220
006200071220     c                   exfmt     or10d01
006300071220     c                   eval      *in28 = *off
006400071220
006500071220      * f3=fine
006600071220     c   kc              leave
006700071220
006800071220      * controllo video
006900071220     c                   exsr      sr_contrd01
007000071227     c   28              iter
007100071220
007200071220
007300071220      * f6=conferma
007400071220     c                   if        *inkf
007500071220     c                   eval      kcoaz = 'OR11'
007600071220     c                   eval      kpjbu = fior11ds
007700071220     c                   call      'BCH10'
007800071220     c                   parm                    kpjba
007900071220     c                   leave
008000071220     c                   endif
008100071220
008200071220     c                   enddo
008300071220
008400071220     c                   eval      *inlr = *on
008500071220
008600071220      *------------------------------------------------------------------------*
008700071220      * controllo video
008800071220      *------------------------------------------------------------------------*
008900071220     c     sr_contrd01   begsr
009000071220
009100071227     c                   setoff                                       010203
009200071220     c                   setoff                                       040708
009300071227     c                   setoff                                       0910
009400071220
009500071220      *?controllo filiale gestione
009600071220      * se valida   ?
009700071220     c                   if        vidpoa <> *zeros
009800071220     c     vidpoa        chain     azorg01l
009900071220     c                   if        not %found(azorg01l)
010000071220     c                   eval      *in07 = *on
010100071220     c                   eval      *in28 = *on
010200071220     c                   eval      vidmsg = msg(01)
010300071220     c                   leavesr
010400071220     c                   endif
010500071220     c                   eval      viddep = orgdes
010600071220     c                   endif
010700071220      * se non immessa imposto la descrizione giusta
010800071220     c                   if        vidpoa = *zeros
010900080108     c                   eval      viddep = '0=Tutte'
011000071220     c                   endif
011100071220
011200071220      *?controllo la scelta
011300071227      * obbligatoria
011400071220     c                   if        vidse1 = *blanks and vidse2 = *blanks
011500080114     c                             and vidse3 = *blanks
011600071227     c                   eval      *in01 = *on
011700071227     c                   eval      *in28 = *on
011800071227     c                   eval      vidmsg = msg(03)
011900071227     c                   leavesr
012000071227     c                   endif
012100071227      * possibile solo una
012200080114     c                   if        (vidse1 = '1' and vidse2 = '1') or
012300080114     c                             (vidse1 = '1' and vidse3 = '1') or
012400080114     c                             (vidse2 = '1' and vidse3 = '1')
012500071227     c                   eval      *in01 = *on
012600071227     c                   eval      *in28 = *on
012700071227     c                   eval      vidmsg = msg(04)
012800071227     c                   leavesr
012900071227     c                   endif
013000071227      * non è possibile valorizzare i campi senza selezione
013100071227      * scelta 1 e cliente da
013200071227     c                   if        vidse1 = *blanks and vidcro <> *zeros
013300071227     c                   eval      *in03 = *on
013400071227     c                   eval      *in28 = *on
013500071227     c                   eval      vidmsg = msg(05)
013600071227     c                   leavesr
013700071227     c                   endif
013800071227      * scelta 1 e cliente a
013900071227     c                   if        vidse1 = *blanks and vidcro2 <> *zeros
014000071227     c                   eval      *in10 = *on
014100071227     c                   eval      *in28 = *on
014200071227     c                   eval      vidmsg = msg(05)
014300071227     c                   leavesr
014400071227     c                   endif
014500071227      * scelta 2 e ragione sociale
014600071227     c                   if        vidse2 = *blanks and vidrsr <> *blanks
014700071227     c                   eval      *in04 = *on
014800071227     c                   eval      *in28 = *on
014900071227     c                   eval      vidmsg = msg(05)
015000071227     c                   leavesr
015100071227     c                   endif
015200071220
015300071227      *?controllo cliente
015400071227      * cliente da > a
015500071227     c                   if        (vidcro <> *zeros or vidcro2 <> *zeros) and
015600071227     c                              vidcro > vidcro2
015700071227     c                   eval      *in03 = *on
015800071227     c                   eval      *in28 = *on
015900071227     c                   eval      vidmsg = msg(06)
016000071227     c                   leavesr
016100071227     c                   endif
016200071227
016300071227      *?controllo giro
016400071227     c                   eval      viddescgi = '_=Tutti'
016500071227      * ricerca giro
016600071227     c                   if        %scan('?':vidcgi) > *zeros
016700071227     c                   clear                   fidg09ds
016800071227     c                   eval      d09iop0 = 'P01'
016900071227     c                   if        vidpocgi = *zeros
017000071227     c                   eval      d09ifgs = dutpou
017100071227     c                   else
017200071227     c                   eval      d09ifgs = vidpocgi
017300071227     c                   endif
017400071227     c                   eval      d09idat = *date
017500071227     c                   eval      d09itug = 'R'
017600071227     c                   eval      kpjbu = fidg09ds
017700071227     c                   call      'FIDG09R'
017800071227     c                   parm                    kpjba
017900071227     c                   eval      fidg09ds = kpjbu
018000071227      * imposto il giro scelto
018100071227     c                   eval      vidpocgi = d09ofgs
018200071227     c                   eval      vidcgi = d09ocgi
018300071227     c                   eval      viddescgi = d09odes
018400071227     c                   eval      *in08 = *on
018500071227     c                   leavesr
018600071227     c                   endif
018700071227      * controllo
018800071227     c                   if        vidcgi <> *blank
018900071227      * filiale giro obbligatoria
019000071227     c                   if        vidpocgi = *zeros
019100071227     c                   eval      *in02 = *on
019200071227     c                   eval      *in28 = *on
019300071227     c                   eval      vidmsg = msg(02)
019400071227     c                   leavesr
019500071227     c                   endif
019600071227     c                   clear                   fidg09ds
019700071227     c                   eval      d09iop0 = '001'
019800071227     c                   eval      d09ifgs = vidpocgi
019900071227     c                   eval      d09idat = *date
020000071227     c                   eval      d09icgi = vidcgi
020100071227     c                   eval      d09itug = 'R'
020200071227     c                   eval      kpjbu = fidg09ds
020300071227     c                   call      'FIDG09R'
020400071227     c                   parm                    kpjba
020500071227     c                   eval      fidg09ds = kpjbu
020600071227      * giro errato
020700071227     c                   if        d09oerr = '1'
020800071227     c                   eval      *in08 = *on
020900071227     c                   eval      *in28 = *on
021000071227     c                   eval      vidmsg = msg(07)
021100071227     c                   leavesr
021200071227     c                   endif
021300071227      * imposto la descrizione del giro
021400071227     c                   eval      viddescgi = d09odes
021500071227     c                   endif
021600080114      * se richiesto ordinamento per giro la filiale giro è obbligatoria
021700080114     c                   if        vidse3 = '1' and vidpocgi = *zeros
021800080114     c                   eval      *in02 = *on
021900080114     c                   eval      *in28 = *on
022000080114     c                   eval      vidmsg = msg(08)
022100080114     c                   leavesr
022200080114     c                   endif
022300071220
022400071227      *?controllo codice piano dei conti
022500071227     c                   eval      viddesksc = '_=Tutti'
022600071227     c                   if        vidksc <> *zeros
022700071227     c                   clear                   tibs69ds
022800071227     c                   clear                   ds_cnaco
022900071227     c                   clear                   ds_cnind
023000071227     c                   clear                   ds_cnclp
023100071227     c                   clear                   ds_fncls
023200071227     c                   eval      i69kac = vidksc
023300071227     c                   call      'TIBS69R'
023400071227     c                   parm                    tibs69ds
023500071227     c                   parm                    ds_cnaco
023600071227     c                   parm                    ds_cnind
023700071227     c                   parm                    ds_cnclp
023800071227     c                   parm                    ds_fncls
023900071227      * errore
024000071227     c                   if        o69err = *on
024100071227     c                   eval      *in09 = *on
024200071227     c                   eval      *in28 = *on
024300071227     c                   eval      vidmsg = o69msg
024400071227     c                   leavesr
024500071227     c                   endif
024600071227     c                   eval      viddesksc = acorag
024700071227     c                   endif
024800071227
024900071227      *?imposto i dati nella ds di passaggio
025000071227      * ordinamento
025100071227     c                   clear                   fior11ds
025200071227     c                   if        vidse1 <> *blanks
025300071227     c                   eval      r11ord = '1'
025400071227     c                   endif
025500071227     c                   if        vidse2 <> *blanks
025600071227     c                   eval      r11ord = '2'
025700071227     c                   endif
025800080118     c                   if        vidse3 <> *blanks
025900080118     c                   eval      r11ord = '3'
026000080118     c                   endif
026100071227      * cliente ritiro
026200071227     c                   eval      r11crada = vidcro
026300071227     c                   eval      r11craal = vidcro2
026400071227      * ragione sociale
026500071227     c                   eval      r11rsr = vidrsr
026600071227      * filiale appartenenza
026700071227     c                   eval      r11poa = vidpoa
026800071227      * tipo anagrafica
026900071227     c                   eval      r11tac = vidtac
027000071227      * frequenza ritiro
027100071227     c                   eval      r11tcr = vidtcr
027200071227      * solo clienti con giro
027300071227     c                   eval      r11sol = vidsol
027400071227      * filiale giro
027500071227     c                   eval      r11pocgi = vidpocgi
027600071227      * codice giro
027700071227     c                   eval      r11cgi = vidcgi
027800071227      * codice piano dei conti
027900071227     c                   eval      r11ksc = vidksc
028000080108      * filiale utente
028100080108     c                   eval      r11fil = dutpou
028200080108      * procedura attiva
028300080108     c                   if        *in15
028400080108     c                   eval      r11geo = 'S'
028500080108     c                   endif
028600150910      * Filiale ritiro diversa
028700150910     c                   eval      r11fdiff = VIDfdiff
028800071220
028900071227     c                   endsr
029000071220
029100071220      *------------------------------------------------------------------------*
029200071220      *   Operazioni iniziali      ?
029300071220      *------------------------------------------------------------------------*
029400071220     c     *inzsr        begsr
029500071220
029600071220     c     *entry        plist
029700071220     c                   parm                    kpjba
029800071220
029900071220     c     *dtaara       define    §azute        azuteds
030000071220     c     *dtaara       define    §datiute      ddatiute
030100071220     c                   in(e)     *dtaara
030200071220     c                   if        %error or rsut = *blanks
030300071220     c                   clear                   tibs34ds
030400071220     c                   call      'TIBS34R'
030500071220     c                   parm                    tibs34ds
030600071220     c                   in        *dtaara
030700071220     c                   endif
030800071220
030900071220      * pulisco i campi del video
031000071220     c                   clear                   vidse1
031100071220     c                   clear                   vidse2
031200080118     c                   clear                   vidse3
031300071220     c                   clear                   vidcro
031400071220     c                   clear                   vidcro2
031500071220     c                   clear                   vidrsr
031600071220     c                   clear                   vidtac
031700071220     c                   clear                   vidtcr
031800071227     c                   clear                   vidksc
031900071227     c                   eval      viddesksc = '_=Tutti'
032000071220     c                   clear                   vidsol
032100071227     c                   clear                   vidpocgi
032200071220     c                   clear                   vidcgi
032300071227     c                   eval      viddescgi = '_=Tutti'
032400150910     c                   clear                   vidfdiff
032500071220
032600071220      * filiale gestione da filiale utente
032700071220      * se non è sede
032800071220     c                   if        simfel = 46
032900080108     c                   eval      viddep = '0=Tutte'
033000071220     c                   else
033100071220     c                   eval      vidpoa = dutpou
033200071220     c     vidpoa        chain     azorg01l
033300071220     c                   if        %found(azorg01l)
033400071220     c                   eval      viddep = orgdes
033500071220     c                   endif
033600071220     c                   endif
033700080108
033800080108      * controllo se la filiale utente è partita con GEO ORM
033900080108     c                   clear                   og147
034000080108     c     dutpou        chain     azorg01l
034100080108     c                   if        %found(azorg01l)
034200080108     c                   eval      og147 = orgde7
034300080108     c                   endif
034400080108     c                   if        §ogcgio = 'S' and §ogddao > *zeros
034500080108     c                   eval      *in15 = *on
034600080108     c                   endif
034700071220
034800071220     c                   endsr
034900071220
035000071220      *------------------------------------------------------------------------*
035100071227
035200071227** MSG
035300071220Filiale errata                                                                 01
035400071227Filiale giro obbligatoria se immesso giro di ritiro                            02
035500071227Immettere una Selezione                                                        03
035600071227Immessa Più di una Selezione, Immetterne Solamente una                         04
035700071227Non è Possibile Immettere una Valore se non è stata Immessa una Selezione      05
035800071227Codice Cliente Ritiro Iniziale Maggiore di quello Finale                       06
035900071227Codice Giro non corretto                                                       07
036000080114Filiale giro obbligatoria se richiesto ordinamento per giro ritiro             08
