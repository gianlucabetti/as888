000100010925      *PARMS DYNUSRPRF(*OWNER)
000200991026      /TITLE Download via Internet: decisore traduzione ASCII.
000300990908     H dftactgrp(*yes)
000400991026
000500991026      **********************************************************************
000600991026      * Legenda PRMESITO
000700991026      * ' ' = Elaborazione non eseguita.
000800991026      * '0' = Elaborazione eseguita senza errori.
000900991104      * 'A' = Parametro PRMKSU vuoto.
001000991026      * 'B' = Parametro PRMTIP vuoto o sconosciuto.
001100991026      * 'C' = Tipo file non previsto per il cliente.
001200991104      * 'D' = Cliente non abilitato al servizio.
001300991026      **********************************************************************
001400991026
001500991026     Ftivlt00f  o    e             disk
001600000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
001700991105     Ftivss02l  if   e           k disk
001800990920     Ftivtf01l  if   e           k disk
001900991026     Ftivir02l  if   e           k disk
002000000707     Ftivre01l  if   e           k disk
002100000622     Ftntbe01l  if   e           k disk
002200000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002300000627     F                                     PREFIX(f_)
002400000627
002500020212     D disv          e ds
002600991027     D psds           sds
002700991027     D  psjobname            244    253
002800991027     D  psuser               254    263
002900991027     D  psjobnbr             264    269s 0
003000991026     D prmksc          s                   like(virksc)
003100991026     D prmksu          s                   like(virksc)
003200991026     D prmtip          s                   like(virtip)
003300991026     D prmesito        s              1
003400000623     D prmprog         s                   like(vltprg)
003500000623     D prmsrv          s                   like(vltisv)
003600000628     D prmope          s              1
003700000713     D prmnfp          s                   like(vltfl2)
003800991026     D wrkksc          s                   like(prmksc)
003900991026     D wrkksu          s                   like(prmksu)
004000991026     D wrktip          s                   like(prmtip)
004100991026     D wrkesito        s                   like(prmesito)
004200991026     D wrkoggiiso      s               d
004300991026     D wrkoggi         s              8  0
004400991026     D wrkora          s              6  0
004500991026     D wrkmese         s              2  0
004600000623     D wrkprg          s              8  0 INZ(*zeros)
004700000711     D wrkvarie        s              1    INZ('0')
004800000712     D wrkmsg          s             80    INZ(*blanks)
004900000622     D dwlprg          s             10    INZ(*all'0')
005000000914     D vssok           s               n   INZ(*off)
005100000711
005200991026     D                 ds                  inz
005300991026     D wrkday                  1      2s 0
005400991026     D wrkdaychr                      2    overlay(wrkday)
005500991026     D wrkpgt          s                   like(vtfpgt)
005600991026     D wrkppt          s                   like(vtfppt)
005700000711     D wrkpgi          s                   like(vtfpgi)
005800000711     D wrkppi          s                   like(vtfppi)
005900991026     D wrkfid          s                   like(vtffid)
006000991026     D wrkmbd          s                   like(vltmbd)
006100991026     D wrkfis          s                   like(vtffis)
006200991026     D wrkmbs          s                   like(vltmbs)
006300991026     D wrkfit          s                   like(vtffit)
006400991026     D wrkmbt          s                   like(vltmbd)
006500991026     D prmfid          s                   like(wrkfid)
006600991026     D prmmbd          s                   like(wrkmbd)
006700991026     D prmfis          s                   like(wrkfis)
006800991026     D prmmbs          s                   like(wrkmbs)
006900991026     D prmfit          s                   like(wrkfit)
007000991027     D prmmbt          s                   like(wrkmbt)
007100991026     D prmpgt          s                   like(wrkpgt)
007200991026     D prmppt          s                   like(wrkppt)
007300991026     D mc              s              1    dim(12) ctdata perrcd(12)
007400991104
007500991111     D mbrd01          s            266
007600991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
007700991111     D mbrd03          s              8    inz('MBRD0200')
007800991111     D mbrd04          ds
007900991111     D mbrd04a                       10                                         File
008000991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
008100991111     D mbrd05          s             10                                         Membro
008200991125     D mbrd06          s              1    inz('0')
008300991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
008400991104     D rneta01         ds
008500991104     D  rneta01a                      8b 0
008600991104     D  rneta01b                      8b 0
008700991104     D  rneta01c                     24
008800991104     D rneta02         s              8b 0 inz(%size(rneta01))
008900991104     D rneta03         s              8b 0 inz(1)
009000991104     D rneta04         s             10    inz('SYSNAME')
009100991104     D nait            ds
009200991104     D  nait01                       10
009300991104     D  nait02                        1
009400991104     D  nait03                        1
009500991104     D  nait04                        8b 0
009600991104     D  sysname                       8
009700991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
009800991104
009900000622
010000000905     D*-------------------
010100000905     D* DS di ridefinizione file di log x passaggio recordset SQL
010200000905     D*-------------------
010300000905     D  dsvlt        E DS                  extname(tivlt00f)
010400000905     D                                     prefix(s_)
010500000905
010600000905
010700000905
010800000622      /TITLE *** MAIN ***
010900000622     C*
011000000906     C                   if        prmnfp  > '01' and
011100000906     C                             prmprog > *blanks
011200000905     C                   exsr      rtvdatora
011300000905     C                   else
011400000905     C                   time                    wrkoggiiso
011500000905     C                   time                    wrkora
011600000905     C     *iso          move      wrkoggiiso    wrkoggi
011700000905     C                   endif
011800000721     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
011900000328     C* Inizializzo variabili "wrk".
012000000328     C                   exsr      inzwrk
012100991026     C* Controllo parametri ricevuti.
012200991026     C                   exsr      chkprm
012300000626     C* Reperisco informazioni file.
012400000706     C                   exsr      rtvleg
012500000706     C* Alla fine...
012600000706     C                   exsr      uscita
012700000626
012800000626
012900000905
013000000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
013100000905     C*
013200000905     C     rtvdatora     begsr
013300000905     C*
013400000905     C/EXEC SQL
013500000905     C+ declare C1 cursor for select *
013600000905     C+ from tivlt00f where
013700000905     C+ vltprg = :prmprog
013800000905     C+ for fetch only
013900000905     C/END-EXEC
014000000905     C
014100000905     C/EXEC SQL
014200000905     C+ open C1
014300000905     C/END-EXEC
014400000905     C
014500000905     C/EXEC SQL
014600000905     C+ Fetch C1 into :dsvlt
014700000905     C/END-EXEC
014800000905     C*
014900000906     C                   if        sqlcod = *zeros     and
015000000906     C                             s_vltprg  = prmprog and
015100000906     C                             s_vltdat  > *zeros  and
015200000905     C                             s_vltora  > *zeros
015300000905     C                   z-add     s_vltdat      wrkoggi
015400000905     C                   z-add     s_vltora      wrkora
015500000905     C                   else
015600000905     C                   time                    wrkoggiiso
015700000905     C                   time                    wrkora
015800000905     C     *iso          move      wrkoggiiso    wrkoggi
015900000905     C                   endif
016000000905     C
016100000905     C/EXEC SQL
016200000905     C+ close C1
016300000905     C/END-EXEC
016400000905     C
016500000905     C*
016600000905     C                   endsr
016700000905
016800000905
016900000706
017000000706      /TITLE Determino eventuali legami cliente/tipi file
017100000706     C     rtvleg        begsr
017200000706     C*
017300000706     C                   eval      virksc = wrkksu
017400000706     C                   eval      virtip = wrktip
017500000706     C                   eval      virdti = wrkoggi
017600000707     C                   eval      vtftip = wrktip
017700000706     C* Reperisco informazioni file.
017800000706     C                   exsr      rtvtip
017900000706     C*
018000000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
018100000713     C                   if        wrkesito = '0'     and
018200000713     C                             prmprog  = *blanks and
018300000713     C                             prmope   = *blanks
018400000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
018500000711     C                   movel     '1'           wrkvarie
018600000706     C*
018700000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
018800000707     C     k03vir02      setll     tivre01l
018900000707     C                   if        %found(tivre01l)
019000000707     C     k03vir02      reade     tivre01l
019100000706     C                   dow       not %eof
019200000706     C                   eval      virksc = wrkksu
019300000706     C                   eval      virtip = vretfl
019400000706     C                   eval      virdti = wrkoggi
019500000707     C                   eval      vtftip = vretfl
019600000712     C                   eval      wrktip = vretfl
019700000706     C* Reperisco informazioni file.
019800000706     C                   exsr      rtvtip
019900000712     C                   eval      virtip = vretip
020000000707     C     k03vir02      reade     tivre01l
020100000706     C                   enddo
020200000706     C                   endif
020300000706     C*
020400000706     C                   endif
020500000711     C* Eseguo a prescindere dal risultato il PGM "post-traduzione"
020600000711     C                   exsr      exepgm
020700000706     C*
020800000706     C                   endsr
020900000706
021000000706
021100000706
021200000706      /TITLE Reperisco tipo file.
021300000706     C     rtvtip        begsr
021400000706     C* Reperisco il tipo file.
021500000706     C     k01vtf01      chain     tivtf01l
021600000706     C* Se il tipo file non esiste
021700000706     C* oppure non è valido per il download.
021800000706     C                   if        not %found
021900000706     C                             or vtfud <> 'D'
022000000706     C                   eval      wrkesito = 'B'
022100000721     C                   else
022200000706     C* Reperisco il tipo file del cliente.
022300000706     C     k03vir02      setgt     tivir02l
022400000706     C     k02vir02      readpe    tivir02l
022500000706     C* Tipo file non previsto per il cliente
022600000706     C* oppure scaduto.
022700000731     C                   if        %eof
022800000706     C                             or virdtf < wrkoggi
022900000706     C                             or virdti > wrkoggi
023000000706     C*
023100000731     C                   exsr      inztivltp
023200000706     C*
023300000706     C                   eval      vltsts = '2'
023400000706     C                   eval      vltmsg =
023500000706     C                               %trim(sysname)
023600000706     C                             + '/'
023700000706     C                             + %trim(psjobname)
023800000706     C                             + '/'
023900000706     C                             + %trim(psuser)
024000000706     C                             + '/'
024100000706     C                             + %trim(%editc(psjobnbr:'Z'))
024200000706     C                             + ' Tipo file non previsto per il cliente.'
024300000706     C*
024400000706     C                   exsr      wrttivlt
024500000706     C                   eval      wrkesito = 'C'
024600000706     C*
024700000706     C                   else
024800000706     C*
024900000706     C* Imposto i default.
025000000706     C                   if        virpgt <> *blanks
025100000706     C                   eval      wrkpgt = virpgt
025200000706     C                   eval      wrkppt = virppt
025300000706     C                   else
025400000706     C                   eval      wrkpgt = vtfpgt
025500000706     C                   eval      wrkppt = vtfppt
025600000706     C                   endif
025700000712     C*
025800000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
025900000711     C                   if        wrkvarie = '0'                               <===================
026000000711     C                   if        virpgi <> *blanks                            <===================
026100000711     C                   eval      wrkpgi = virpgi                              <===================
026200000711     C                   eval      wrkppi = virppi                              <===================
026300000711     C                   else                                                   <===================
026400000711     C                   eval      wrkpgi = vtfpgi                              <===================
026500000711     C                   eval      wrkppi = vtfppi                              <===================
026600000711     C                   endif                                                  <===================
026700000711     C                   endif                                                  <===================
026800000706     C* File da tradurre.
026900000706     C                   if        virfid <> *blanks
027000000706     C                   eval      wrkfid = virfid
027100000706     C                   else
027200000706     C                   eval      wrkfid = vtffid
027300000706     C                   endif
027400000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
027500000706     C* File storico.
027600000706     C                   if        virfis <> *blanks
027700000706     C                   eval      wrkfis = virfis
027800000706     C                   else
027900000706     C                   eval      wrkfis = vtffis
028000000706     C                   endif
028100000706     C                   extrct    wrkoggiiso:*m wrkmese
028200000706     C                   extrct    wrkoggiiso:*d wrkday
028300000706     C                   eval      wrkmbs =
028400000706     C                             mc(wrkmese)
028500000706     C                             + wrkdaychr
028600000706     C                             + %subst(wrkksu:2:7)
028700000706     C* File tradotto.
028800000706     C                   if        virfit <> *blanks
028900000706     C                   eval      wrkfit = virfit
029000000706     C                   else
029100000706     C                   eval      wrkfit = vtffit
029200000706     C                   endif
029300000706     C                   eval      wrkmbt = 'M' + wrkksu
029400000706     C*
029500000706     C                   eval      vssisv = virfi1
029600000706     C* Valorizzo progressivo applicazione.
029700000706     C                   exsr      calprog
029800000706     C* Controllo abilitazione al servizio.
029900000706     C                   exsr      chkvss
030000000706     C                   endif
030100000706     C                   endif
030200000706     C*
030300000706     C                   endsr
030400000706
030500000711
030600000711
030700000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
030800000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
030900000711     C*
031000000711     C     exepgm        begsr
031100000711     C* Lancio il pgm in variabile con relativi parametri
031200000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
031300000711    C                   if        wrkpgi <> *blanks
031400000712     C* Chiamo il pgm post traduzione.
031500000712     C                   call(e)   'TIS7T9C1'
031600000712     C                   parm      wrkfid        prmfid
031700000712     C                   parm      wrkmbd        prmmbd
031800000712     C                   parm      wrkfis        prmfis
031900000712     C                   parm      wrkmbs        prmmbs
032000000712     C                   parm                    wrkpgi
032100000712     C                   parm                    wrkppi
032200000712     C                   parm      *blanks       prmesito
032300000712     C* Se la call è finita in errore
032400000712     C* o il lancio del PGM ha avuto degli errori.
032500000712     C                   if        %error
032600000712     C                             or prmesito <> '0'
032700000712     C     k01vlt        setll     tivlt07l
032800000712     C                   if        %found(tivlt07l)
032900000712     C     k01vlt        reade     tivlt07l
033000000712     C                   dow       not %eof
033100000712     C                   if        vltsqz = *zeros and
033200000712     C                             (vltsts = '0' or vltsts = '1')
033300000712     C                   eval      vltsts = '2'
033400000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
033500000712     C                   eval      vltmsg =
033600000712     C                               %trim(sysname)
033700000712     C                             + '/'
033800000712     C                             + %trim(psjobname)
033900000712     C                             + '/'
034000000712     C                             + %trim(psuser)
034100000712     C                             + '/'
034200000712     C                             + %trim(%editc(psjobnbr:'Z'))
034300000712     C                             + %trim(wrkmsg)
034400000712     C                   update    tivlt700
034500000712     C                   endif
034600000712     C     k01vlt        reade     tivlt07l
034700000712     C                   enddo
034800000712     C                   endif
034900000712     C                   endif
035000000712     C*
035100000711     C                   endif
035200000711     C*
035300000711     C                   endsr
035400000711
035500000711
035600000626
035700000626      /TITLE Valorizzazione Progressivo Applicazione
035800000626     C     calprog       begsr
035900000626     C*
036000000706     C                   if        dwlprg = *all'0'
036100000627     C                   read(e)   tis7prgf
036200000627     C                   if        not %error
036300000627     C                   eval      dwlprg = f_tis7prgf
036400000627     C*
036500000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
036600000719     C                   if        prmsrv <> *blanks
036700000719     C                   eval      vssisv = prmsrv
036800000719     C                   endif
036900000626     C*
037000000719     C                   if        prmprog = *blanks
037100000626     C                   move(p)   dwlprg        wrkprg
037200000626     C                   add       1             wrkprg
037300000626     C                   move(p)   wrkprg        dwlprg
037400000731     C                   movel     vssisv        dwlprg
037500000626     C                   else
037600000626     C                   eval      dwlprg = prmprog
037700000626     C                   endif
037800000627     C*
037900000627     C                   eval      f_tis7prgf = dwlprg
038000000627     C                   update    tis7prg0
038100000627     C                   endif
038200000706     C                   endif
038300000626     C*
038400000626     C                   endsr
038500991104
038600000706
038700000706
038800000706
038900991104      /TITLE Controllo abilitazione servizio.
039000991104     C     chkvss        begsr
039100991104     C*
039200000914     C                   eval      vssok = *off
039300000914     C*
039400991105     C                   eval      vssksu = wrkksu
039500000914     C     k02vss02      setll     tivss02l
039600000914     C     k02vss02      reade     tivss02l
039700000914     C                   dow       not %eof
039800000914     C                   if        vssdde <= wrkoggi and
039900000914     C                             vssdsc >= wrkoggi and
040000000914     C                             vssvat <> 'A'     and
040100000914     C                             vssvat <> 'S'
040200000914     C                   eval      vssok = *on
040300000914     C                   leave
040400000914     C                   endif
040500000914     C     k02vss02      reade     tivss02l
040600000914     C                   enddo
040700000914     C*
040800000914     C                   if        vssok = *off
040900991104     C*
041000000622     C                   exsr      inztivlt
041100000329     C                   eval      vltsts = '2'
041200991104     C                   eval      vltmsg =
041300991104     C                               %trim(sysname)
041400991104     C                             + '/'
041500991104     C                             + %trim(psjobname)
041600991104     C                             + '/'
041700991104     C                             + %trim(psuser)
041800991104     C                             + '/'
041900991104     C                             + %trim(%editc(psjobnbr:'Z'))
042000991104     C                             + ' Cliente non abilitato al servizio.'
042100000622     C                   exsr      wrttivlt
042200000622     C*
042300000622     C                   eval      wrkesito = 'D'
042400000622     C                   else
042500000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
042600000706     C                   exsr      traduci
042700991104     C*
042800991104     C                   endif
042900991104     C*
043000991104     C                   endsr
043100000622
043200000622
043300000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
043400000622     C     rtvtia        begsr
043500000622     C*
043600000622     C                   eval      tbecod = 'ISV'
043700000731     C                   eval      tbeke1 = virfi1
043800000622     C     keytbe        chain     tntbe01l
043900000622     C                   if        %found(tntbe01l)
044000020212     C                   movel     tbeuni        disv
044100000622     C                   endif
044200000622     C*
044300000622     C                   endsr
044400000706
044500000706
044600000706
044700000706      /TITLE Traduzione.
044800000706     C     traduci       begsr
044900000706     C* Reperisco informazioni membro da tradurre.
045000000706     C                   eval      mbrd04a = wrkfid
045100000706     C                   eval      mbrd05  = wrkmbd
045200000706     C                   exsr      rtvmbrd
045300000706     C* Controllo se il membro da tradurre ha dei record.
045400000904     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
045500000904     C                   if        (qusei=*blanks and
045600000904     C                             qusnbrcr > 0) or prmnfp > '01'
045700000706     C*
045800000706     C                   exsr      inztivlt
045900000706     C* Chiamo il pgm traduttore.
046000000706     C                   call(e)   'TIS7T9C'
046100000706     C                   parm      wrkfid        prmfid
046200000706     C                   parm      wrkmbd        prmmbd
046300000706     C                   parm      wrkfis        prmfis
046400000706     C                   parm      wrkmbs        prmmbs
046500000706     C                   parm      wrkfit        prmfit
046600000706     C                   parm      wrkmbt        prmmbt
046700000706     C                   parm      wrkpgt        prmpgt
046800000706     C                   parm      wrkppt        prmppt
046900000706     C                   parm      *blanks       prmesito
047000000706     C* Se la call è finita in errore
047100000706     C* o la traduzione ha avuto degli errori.
047200000706     C                   if        %error
047300000712     C                             or prmesito <> '0'
047400000706     C                   eval      vltsts = '2'
047500000706     C                   eval      vltmsg =
047600000706     C                               %trim(sysname)
047700000706     C                             + '/'
047800000706     C                             + %trim(psjobname)
047900000706     C                             + '/'
048000000706     C                             + %trim(psuser)
048100000706     C                             + '/'
048200000706     C                             + %trim(%editc(psjobnbr:'Z'))
048300000706     C                             + ' Errori nella traduzione.'
048400000706     C                   else
048500000706     C                   eval      wrkesito = '0'
048600000706     C                   endif
048700000706     C*
048800000706     C                   exsr      wrttivlt
048900000706     C*
049000000706     C                   endif
049100000706     C*
049200000706     C                   endsr
049300000328
049400000706
049500000706
049600000328      /TITLE Inizializzo variabili "wrk".
049700000328     C     inzwrk        begsr
049800000328     C*
049900000328     C                   clear                   wrkfit
050000000328     C                   clear                   wrkmbt
050100000328     C                   clear                   wrkpgi
050200000328     C                   clear                   wrkppi
050300000328     C                   clear                   wrkfis
050400000328     C                   clear                   wrkmbs
050500000328     C*
050600000328     C                   endsr
050700991026
050800000706
050900000706
051000991026      /TITLE Inizializzo TIVLT00F.
051100991026     C     inztivlt      begsr
051200000804     C*
051300000804     C                   exsr      rtvtia
051400991026     C*
051500000328     C                   clear                   tivlt000
051600991026     C                   eval      vltksc = wrkksc
051700991026     C                   eval      vltksu = wrkksu
051800000721     C                   eval      vlttip = wrktip
051900991026     C                   eval      vltfld = wrkfit
052000991026     C                   eval      vltmbd = wrkmbt
052100991026     C                   eval      vltpgi = wrkpgi
052200991026     C                   eval      vltppi = wrkppi
052300020212     C                   if        §isvproc = 'F'
052400000804     C                   eval      vltsnd = 'D'
052500000804     C                   else
052600991026     C                   eval      vltsnd = vtfsnd
052700000804     C                   endif
052800991026     C                   eval      vltfls = wrkfis
052900991026     C                   eval      vltmbs = wrkmbs
053000991026     C                   eval      vltdat = wrkoggi
053100991115     C                   eval      vltora = wrkora
053200991026     C                   eval      vltsts = '0'
053300991027     C                   eval      vltmsg =
053400991104     C                               %trim(sysname)
053500991104     C                             + '/'
053600991104     C                             + %trim(psjobname)
053700991027     C                             + '/'
053800991027     C                             + %trim(psuser)
053900991027     C                             + '/'
054000991027     C                             + %trim(%editc(psjobnbr:'Z'))
054100000328     C                   z-add     *zeros        vltsqz
054200000731     C*
054300991026     C                   endsr
054400991026
054500000706
054600000731
054700000731      /TITLE Inizializzo parzialmente TIVLT00F.
054800000731     C     inztivltp     begsr
054900000731     C*
055000000731     C                   clear                   tivlt000
055100000731     C                   eval      vltksc = wrkksc
055200000731     C                   eval      vltksu = wrkksu
055300000731     C                   eval      vlttip = wrktip
055400000731     C                   eval      vltpgi = wrkpgi
055500000731     C                   eval      vltppi = wrkppi
055600000731     C                   eval      vltsnd = vtfsnd
055700000731     C                   eval      vltdat = wrkoggi
055800000731     C                   eval      vltora = wrkora
055900000731     C                   eval      vltsts = '0'
056000000731     C                   eval      vltmsg =
056100000731     C                               %trim(sysname)
056200000731     C                             + '/'
056300000731     C                             + %trim(psjobname)
056400000731     C                             + '/'
056500000731     C                             + %trim(psuser)
056600000731     C                             + '/'
056700000731     C                             + %trim(%editc(psjobnbr:'Z'))
056800000731     C                   z-add     *zeros        vltsqz
056900000731     C*
057000000731     C                   exsr      rtvtia
057100000731     C*
057200000731     C                   endsr
057300000731
057400000731
057500000706
057600991026      /TITLE Scrittura TIVLT00F.
057700000731     C     wrttivlt      begsr
057800991026     C*
057900000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
058000000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
058100000731     C                   eval      vltisv = virfi1
058200020212     C                   eval      vlttia = §isvproc
058300000622     C                   eval      vltprg = dwlprg
058400000713     C                   eval      vltfl2 = prmnfp
058500991026     C                   write     tivlt000
058600991026     C*
058700991026     C                   endsr
058800991026
058900000706
059000000706
059100991026      /TITLE Operazioni finali.
059200991026     C     uscita        begsr
059300991026     C*
059400000627     C                   seton                                        lr
059500991026     C*
059600991026     C                   endsr
059700991026
059800000706
059900000706
060000991026      /TITLE Controllo formale parametri ricevuti.
060100991026     C     chkprm        begsr
060200991026     C*
060300991104     C                   if        wrkksu = *blanks
060400991026     C                   eval      wrkesito = 'A'
060500991026     C                   exsr      uscita
060600991026     C                   endif
060700991026     C*
060800991026     C                   if        wrktip = *blanks
060900991026     C                   eval      wrkesito = 'B'
061000991026     C                   exsr      uscita
061100991026     C                   endif
061200991026     C*
061300991026     C                   clear                   wrkesito
061400991026     C*
061500991026     C                   endsr
061600991111
061700000706
061800000706
061900991111      /TITLE Reperisco informazioni membro.
062000991111     C     rtvmbrd       begsr
062100991111     C* Pulisco e imposto la DS degli errori.
062200000124     C                   clear                   qusec
062300991111     C                   eval      qusbprv = %size(qusec)
062400991111     C*
062500991111     C                   call      'QUSRMBRD'
062600991111     C     qusm0200      parm                    mbrd01
062700991111     C                   parm                    mbrd02
062800991111     C                   parm                    mbrd03
062900991111     C                   parm                    mbrd04
063000991111     C                   parm                    mbrd05
063100991111     C                   parm                    mbrd06
063200991111     C                   parm                    qusec
063300991111     C*
063400991111     C                   endsr
063500990910
063600000706
063700000706
063800990908      /TITLE Operazioni iniziali.
063900990908     C     *inzsr        begsr
064000990908     C*
064100990908     C     *entry        plist
064200991026     C     wrkksc        parm                    prmksc
064300991026     C     wrkksu        parm      wrkksu        prmksu
064400991026     C     wrktip        parm                    prmtip
064500991026     C                   parm      wrkesito      prmesito
064600000623     C                   parm                    prmprog
064700000623     C                   parm                    prmsrv
064800000628     C                   parm                    prmope
064900000713     C                   parm                    prmnfp
065000991104     C*
065100991105     C     k02vss02      klist
065200991105     C                   kfld                    vssksu
065300991104     C                   kfld                    vssisv
065400991026     C*
065500991026     C     k03vir02      klist
065600991026     C                   kfld                    virksc
065700991026     C                   kfld                    virtip
065800991026     C                   kfld                    virdti
065900991026     C*
066000991026     C     k02vir02      klist
066100991026     C                   kfld                    virksc
066200991026     C                   kfld                    virtip
066300000706     C*
066400000706     C     k04vir02      klist
066500000706     C                   kfld                    vreksc
066600000706     C                   kfld                    vretfl
066700000706     C                   kfld                    vredti
066800990920     C*
066900990920     C     k01vtf01      klist
067000990920     C                   kfld                    vtftip
067100000622     C*
067200000622     C     keytbe        klist
067300000622     C                   kfld                    tbecod
067400000622     C                   kfld                    tbeke1
067500000712     C*
067600000712     C     k01vlt        klist
067700000712     C                   kfld                    vltprg
067800000712     C                   kfld                    vltksc
067900000706     C*
068000991104     C* Reperisco nome sistema.
068100991104     C                   eval      qusbprv = %size(qusec)
068200991104     C                   call      'QWCRNETA'
068300991104     C                   parm                    rneta01
068400991104     C                   parm                    rneta02
068500991104     C                   parm                    rneta03
068600991104     C                   parm                    rneta04
068700991104     C                   parm                    qusec
068800991104     C                   eval      nait = rneta01c
068900990908     C*
069000990908     C                   endsr
069100991026** ctdata mc
069200991026ABCDEFGHILMN
