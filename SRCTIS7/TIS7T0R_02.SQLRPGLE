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
020500030512     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
020600030512     C* dalla procedura FTP/e-mail)
020700030512     C                   if        chkFTP <> '1'
020800000711     C                   exsr      exepgm
020900030512     C                   endif
021000000706     C*
021100000706     C                   endsr
021200000706
021300000706
021400000706
021500000706      /TITLE Reperisco tipo file.
021600000706     C     rtvtip        begsr
021700000706     C* Reperisco il tipo file.
021800000706     C     k01vtf01      chain     tivtf01l
021900000706     C* Se il tipo file non esiste
022000000706     C* oppure non è valido per il download.
022100000706     C                   if        not %found
022200000706     C                             or vtfud <> 'D'
022300000706     C                   eval      wrkesito = 'B'
022400000721     C                   else
022500000706     C* Reperisco il tipo file del cliente.
022600000706     C     k03vir02      setgt     tivir02l
022700000706     C     k02vir02      readpe    tivir02l
022800000706     C* Tipo file non previsto per il cliente
022900000706     C* oppure scaduto.
023000000731     C                   if        %eof
023100000706     C                             or virdtf < wrkoggi
023200000706     C                             or virdti > wrkoggi
023300000706     C*
023400000731     C                   exsr      inztivltp
023500000706     C*
023600000706     C                   eval      vltsts = '2'
023700000706     C                   eval      vltmsg =
023800000706     C                               %trim(sysname)
023900000706     C                             + '/'
024000000706     C                             + %trim(psjobname)
024100000706     C                             + '/'
024200000706     C                             + %trim(psuser)
024300000706     C                             + '/'
024400000706     C                             + %trim(%editc(psjobnbr:'Z'))
024500000706     C                             + ' Tipo file non previsto per il cliente.'
024600000706     C*
024700000706     C                   exsr      wrttivlt
024800000706     C                   eval      wrkesito = 'C'
024900000706     C*
025000000706     C                   else
025100000706     C*
025200000706     C* Imposto i default.
025300000706     C                   if        virpgt <> *blanks
025400000706     C                   eval      wrkpgt = virpgt
025500000706     C                   eval      wrkppt = virppt
025600000706     C                   else
025700000706     C                   eval      wrkpgt = vtfpgt
025800000706     C                   eval      wrkppt = vtfppt
025900000706     C                   endif
026000000712     C*
026100000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
026200000711     C                   if        wrkvarie = '0'                               <===================
026300000711     C                   if        virpgi <> *blanks                            <===================
026400000711     C                   eval      wrkpgi = virpgi                              <===================
026500000711     C                   eval      wrkppi = virppi                              <===================
026600000711     C                   else                                                   <===================
026700000711     C                   eval      wrkpgi = vtfpgi                              <===================
026800000711     C                   eval      wrkppi = vtfppi                              <===================
026900000711     C                   endif                                                  <===================
027000000711     C                   endif                                                  <===================
027100000706     C* File da tradurre.
027200000706     C                   if        virfid <> *blanks
027300000706     C                   eval      wrkfid = virfid
027400000706     C                   else
027500000706     C                   eval      wrkfid = vtffid
027600000706     C                   endif
027700000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
027800000706     C* File storico.
027900000706     C                   if        virfis <> *blanks
028000000706     C                   eval      wrkfis = virfis
028100000706     C                   else
028200000706     C                   eval      wrkfis = vtffis
028300000706     C                   endif
028400000706     C                   extrct    wrkoggiiso:*m wrkmese
028500000706     C                   extrct    wrkoggiiso:*d wrkday
028600000706     C                   eval      wrkmbs =
028700000706     C                             mc(wrkmese)
028800000706     C                             + wrkdaychr
028900000706     C                             + %subst(wrkksu:2:7)
029000000706     C* File tradotto.
029100000706     C                   if        virfit <> *blanks
029200000706     C                   eval      wrkfit = virfit
029300000706     C                   else
029400000706     C                   eval      wrkfit = vtffit
029500000706     C                   endif
029600000706     C                   eval      wrkmbt = 'M' + wrkksu
029700000706     C*
029800000706     C                   eval      vssisv = virfi1
029900000706     C* Valorizzo progressivo applicazione.
030000000706     C                   exsr      calprog
030100000706     C* Controllo abilitazione al servizio.
030200000706     C                   exsr      chkvss
030300000706     C                   endif
030400000706     C                   endif
030500000706     C*
030600000706     C                   endsr
030700000706
030800000711
030900000711
031000000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
031100000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
031200000711     C*
031300000711     C     exepgm        begsr
031400000711     C* Lancio il pgm in variabile con relativi parametri
031500000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
031600000711    C                   if        wrkpgi <> *blanks
031700000712     C* Chiamo il pgm post traduzione.
031800000712     C                   call(e)   'TIS7T9C1'
031900000712     C                   parm      wrkfid        prmfid
032000000712     C                   parm      wrkmbd        prmmbd
032100000712     C                   parm      wrkfis        prmfis
032200000712     C                   parm      wrkmbs        prmmbs
032300000712     C                   parm                    wrkpgi
032400000712     C                   parm                    wrkppi
032500000712     C                   parm      *blanks       prmesito
032600000712     C* Se la call è finita in errore
032700000712     C* o il lancio del PGM ha avuto degli errori.
032800000712     C                   if        %error
032900000712     C                             or prmesito <> '0'
033000000712     C     k01vlt        setll     tivlt07l
033100000712     C                   if        %found(tivlt07l)
033200000712     C     k01vlt        reade     tivlt07l
033300000712     C                   dow       not %eof
033400000712     C                   if        vltsqz = *zeros and
033500000712     C                             (vltsts = '0' or vltsts = '1')
033600000712     C                   eval      vltsts = '2'
033700000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
033800000712     C                   eval      vltmsg =
033900000712     C                               %trim(sysname)
034000000712     C                             + '/'
034100000712     C                             + %trim(psjobname)
034200000712     C                             + '/'
034300000712     C                             + %trim(psuser)
034400000712     C                             + '/'
034500000712     C                             + %trim(%editc(psjobnbr:'Z'))
034600000712     C                             + %trim(wrkmsg)
034700000712     C                   update    tivlt700
034800000712     C                   endif
034900000712     C     k01vlt        reade     tivlt07l
035000000712     C                   enddo
035100000712     C                   endif
035200000712     C                   endif
035300000712     C*
035400000711     C                   endif
035500000711     C*
035600000711     C                   endsr
035700000711
035800000711
035900000626
036000000626      /TITLE Valorizzazione Progressivo Applicazione
036100000626     C     calprog       begsr
036200000626     C*
036300000706     C                   if        dwlprg = *all'0'
036400000627     C                   read(e)   tis7prgf
036500000627     C                   if        not %error
036600000627     C                   eval      dwlprg = f_tis7prgf
036700000627     C*
036800000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
036900000719     C                   if        prmsrv <> *blanks
037000000719     C                   eval      vssisv = prmsrv
037100000719     C                   endif
037200000626     C*
037300000719     C                   if        prmprog = *blanks
037400000626     C                   move(p)   dwlprg        wrkprg
037500000626     C                   add       1             wrkprg
037600000626     C                   move(p)   wrkprg        dwlprg
037700000731     C                   movel     vssisv        dwlprg
037800000626     C                   else
037900000626     C                   eval      dwlprg = prmprog
038000000626     C                   endif
038100000627     C*
038200000627     C                   eval      f_tis7prgf = dwlprg
038300000627     C                   update    tis7prg0
038400000627     C                   endif
038500000706     C                   endif
038600000626     C*
038700000626     C                   endsr
038800991104
038900000706
039000000706
039100000706
039200991104      /TITLE Controllo abilitazione servizio.
039300991104     C     chkvss        begsr
039400991104     C*
039500000914     C                   eval      vssok = *off
039600000914     C*
039700991105     C                   eval      vssksu = wrkksu
039800000914     C     k02vss02      setll     tivss02l
039900000914     C     k02vss02      reade     tivss02l
040000000914     C                   dow       not %eof
040100000914     C                   if        vssdde <= wrkoggi and
040200000914     C                             vssdsc >= wrkoggi and
040300000914     C                             vssvat <> 'A'     and
040400000914     C                             vssvat <> 'S'
040500000914     C                   eval      vssok = *on
040600000914     C                   leave
040700000914     C                   endif
040800000914     C     k02vss02      reade     tivss02l
040900000914     C                   enddo
041000000914     C*
041100000914     C                   if        vssok = *off
041200991104     C*
041300000622     C                   exsr      inztivlt
041400000329     C                   eval      vltsts = '2'
041500991104     C                   eval      vltmsg =
041600991104     C                               %trim(sysname)
041700991104     C                             + '/'
041800991104     C                             + %trim(psjobname)
041900991104     C                             + '/'
042000991104     C                             + %trim(psuser)
042100991104     C                             + '/'
042200991104     C                             + %trim(%editc(psjobnbr:'Z'))
042300991104     C                             + ' Cliente non abilitato al servizio.'
042400000622     C                   exsr      wrttivlt
042500000622     C*
042600000622     C                   eval      wrkesito = 'D'
042700000622     C                   else
042800000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
042900000706     C                   exsr      traduci
043000991104     C*
043100991104     C                   endif
043200991104     C*
043300991104     C                   endsr
043400000622
043500000622
043600000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
043700000622     C     rtvtia        begsr
043800000622     C*
043900000622     C                   eval      tbecod = 'ISV'
044000000731     C                   eval      tbeke1 = virfi1
044100000622     C     keytbe        chain     tntbe01l
044200000622     C                   if        %found(tntbe01l)
044300020212     C                   movel     tbeuni        disv
044400000622     C                   endif
044500000622     C*
044600000622     C                   endsr
044700000706
044800000706
044900000706
045000000706      /TITLE Traduzione.
045100000706     C     traduci       begsr
045200030512     C*
045300030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
045400030512     C                   call      'TIS7T9R2'
045500030512     C                   parm                    wrkfid
045600030512     C                   parm                    wrkmbd
045700030512     C                   parm      '0'           chkFTP            1
045800030512     C*
045900030512     C* Se nn allocato procedo con l'elaborazione
046000030512     C                   if        chkFTP <> '1'
046100000706     C* Reperisco informazioni membro da tradurre.
046200000706     C                   eval      mbrd04a = wrkfid
046300000706     C                   eval      mbrd05  = wrkmbd
046400000706     C                   exsr      rtvmbrd
046500000706     C* Controllo se il membro da tradurre ha dei record.
046600000904     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
046700000904     C                   if        (qusei=*blanks and
046800000904     C                             qusnbrcr > 0) or prmnfp > '01'
046900000706     C*
047000000706     C                   exsr      inztivlt
047100000706     C* Chiamo il pgm traduttore.
047200000706     C                   call(e)   'TIS7T9C'
047300000706     C                   parm      wrkfid        prmfid
047400000706     C                   parm      wrkmbd        prmmbd
047500000706     C                   parm      wrkfis        prmfis
047600000706     C                   parm      wrkmbs        prmmbs
047700000706     C                   parm      wrkfit        prmfit
047800000706     C                   parm      wrkmbt        prmmbt
047900000706     C                   parm      wrkpgt        prmpgt
048000000706     C                   parm      wrkppt        prmppt
048100000706     C                   parm      *blanks       prmesito
048200000706     C* Se la call è finita in errore
048300000706     C* o la traduzione ha avuto degli errori.
048400000706     C                   if        %error
048500000712     C                             or prmesito <> '0'
048600000706     C                   eval      vltsts = '2'
048700000706     C                   eval      vltmsg =
048800000706     C                               %trim(sysname)
048900000706     C                             + '/'
049000000706     C                             + %trim(psjobname)
049100000706     C                             + '/'
049200000706     C                             + %trim(psuser)
049300000706     C                             + '/'
049400000706     C                             + %trim(%editc(psjobnbr:'Z'))
049500000706     C                             + ' Errori nella traduzione.'
049600000706     C                   else
049700000706     C                   eval      wrkesito = '0'
049800000706     C                   endif
049900000706     C*
050000000706     C                   exsr      wrttivlt
050100000706     C*
050200000706     C                   endif
050300030512     C*
050400030512     C                   else
050500030512     C                   exsr      inztivlt
050600030512     C                   eval      vltsts = '2'
050700030512     C                   eval      vltmsg =
050800030512     C                               %trim(sysname)
050900030512     C                             + '/'
051000030512     C                             + %trim(psjobname)
051100030512     C                             + '/'
051200030512     C                             + %trim(psuser)
051300030512     C                             + '/'
051400030512     C                             + %trim(%editc(psjobnbr:'Z'))
051500030512     C                             + ' Cliente/TipoFile in uso da server FTP'
051600030512     C                   exsr      wrttivlt
051700030512     C                   eval      wrkesito = 'E'
051800030512     C                   endif
051900000706     C*
052000000706     C                   endsr
052100000328
052200000706
052300000706
052400000328      /TITLE Inizializzo variabili "wrk".
052500000328     C     inzwrk        begsr
052600000328     C*
052700000328     C                   clear                   wrkfit
052800000328     C                   clear                   wrkmbt
052900000328     C                   clear                   wrkpgi
053000000328     C                   clear                   wrkppi
053100000328     C                   clear                   wrkfis
053200000328     C                   clear                   wrkmbs
053300000328     C*
053400000328     C                   endsr
053500991026
053600000706
053700000706
053800991026      /TITLE Inizializzo TIVLT00F.
053900991026     C     inztivlt      begsr
054000000804     C*
054100000804     C                   exsr      rtvtia
054200991026     C*
054300000328     C                   clear                   tivlt000
054400991026     C                   eval      vltksc = wrkksc
054500991026     C                   eval      vltksu = wrkksu
054600000721     C                   eval      vlttip = wrktip
054700991026     C                   eval      vltfld = wrkfit
054800991026     C                   eval      vltmbd = wrkmbt
054900991026     C                   eval      vltpgi = wrkpgi
055000991026     C                   eval      vltppi = wrkppi
055100020212     C                   if        §isvproc = 'F'
055200000804     C                   eval      vltsnd = 'D'
055300000804     C                   else
055400991026     C                   eval      vltsnd = vtfsnd
055500000804     C                   endif
055600991026     C                   eval      vltfls = wrkfis
055700991026     C                   eval      vltmbs = wrkmbs
055800991026     C                   eval      vltdat = wrkoggi
055900991115     C                   eval      vltora = wrkora
056000991026     C                   eval      vltsts = '0'
056100991027     C                   eval      vltmsg =
056200991104     C                               %trim(sysname)
056300991104     C                             + '/'
056400991104     C                             + %trim(psjobname)
056500991027     C                             + '/'
056600991027     C                             + %trim(psuser)
056700991027     C                             + '/'
056800991027     C                             + %trim(%editc(psjobnbr:'Z'))
056900000328     C                   z-add     *zeros        vltsqz
057000000731     C*
057100991026     C                   endsr
057200991026
057300000706
057400000731
057500000731      /TITLE Inizializzo parzialmente TIVLT00F.
057600000731     C     inztivltp     begsr
057700000731     C*
057800000731     C                   clear                   tivlt000
057900000731     C                   eval      vltksc = wrkksc
058000000731     C                   eval      vltksu = wrkksu
058100000731     C                   eval      vlttip = wrktip
058200000731     C                   eval      vltpgi = wrkpgi
058300000731     C                   eval      vltppi = wrkppi
058400000731     C                   eval      vltsnd = vtfsnd
058500000731     C                   eval      vltdat = wrkoggi
058600000731     C                   eval      vltora = wrkora
058700000731     C                   eval      vltsts = '0'
058800000731     C                   eval      vltmsg =
058900000731     C                               %trim(sysname)
059000000731     C                             + '/'
059100000731     C                             + %trim(psjobname)
059200000731     C                             + '/'
059300000731     C                             + %trim(psuser)
059400000731     C                             + '/'
059500000731     C                             + %trim(%editc(psjobnbr:'Z'))
059600000731     C                   z-add     *zeros        vltsqz
059700000731     C*
059800000731     C                   exsr      rtvtia
059900000731     C*
060000000731     C                   endsr
060100000731
060200000731
060300000706
060400991026      /TITLE Scrittura TIVLT00F.
060500000731     C     wrttivlt      begsr
060600991026     C*
060700000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
060800000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
060900000731     C                   eval      vltisv = virfi1
061000020212     C                   eval      vlttia = §isvproc
061100000622     C                   eval      vltprg = dwlprg
061200000713     C                   eval      vltfl2 = prmnfp
061300991026     C                   write     tivlt000
061400991026     C*
061500991026     C                   endsr
061600991026
061700000706
061800000706
061900991026      /TITLE Operazioni finali.
062000991026     C     uscita        begsr
062100991026     C*
062200000627     C                   seton                                        lr
062300991026     C*
062400991026     C                   endsr
062500991026
062600000706
062700000706
062800991026      /TITLE Controllo formale parametri ricevuti.
062900991026     C     chkprm        begsr
063000991026     C*
063100991104     C                   if        wrkksu = *blanks
063200991026     C                   eval      wrkesito = 'A'
063300991026     C                   exsr      uscita
063400991026     C                   endif
063500991026     C*
063600991026     C                   if        wrktip = *blanks
063700991026     C                   eval      wrkesito = 'B'
063800991026     C                   exsr      uscita
063900991026     C                   endif
064000991026     C*
064100991026     C                   clear                   wrkesito
064200991026     C*
064300991026     C                   endsr
064400991111
064500000706
064600000706
064700991111      /TITLE Reperisco informazioni membro.
064800991111     C     rtvmbrd       begsr
064900991111     C* Pulisco e imposto la DS degli errori.
065000000124     C                   clear                   qusec
065100991111     C                   eval      qusbprv = %size(qusec)
065200991111     C*
065300991111     C                   call      'QUSRMBRD'
065400991111     C     qusm0200      parm                    mbrd01
065500991111     C                   parm                    mbrd02
065600991111     C                   parm                    mbrd03
065700991111     C                   parm                    mbrd04
065800991111     C                   parm                    mbrd05
065900991111     C                   parm                    mbrd06
066000991111     C                   parm                    qusec
066100991111     C*
066200991111     C                   endsr
066300990910
066400000706
066500000706
066600990908      /TITLE Operazioni iniziali.
066700990908     C     *inzsr        begsr
066800990908     C*
066900990908     C     *entry        plist
067000991026     C     wrkksc        parm                    prmksc
067100991026     C     wrkksu        parm      wrkksu        prmksu
067200991026     C     wrktip        parm                    prmtip
067300991026     C                   parm      wrkesito      prmesito
067400000623     C                   parm                    prmprog
067500000623     C                   parm                    prmsrv
067600000628     C                   parm                    prmope
067700000713     C                   parm                    prmnfp
067800991104     C*
067900991105     C     k02vss02      klist
068000991105     C                   kfld                    vssksu
068100991104     C                   kfld                    vssisv
068200991026     C*
068300991026     C     k03vir02      klist
068400991026     C                   kfld                    virksc
068500991026     C                   kfld                    virtip
068600991026     C                   kfld                    virdti
068700991026     C*
068800991026     C     k02vir02      klist
068900991026     C                   kfld                    virksc
069000991026     C                   kfld                    virtip
069100000706     C*
069200000706     C     k04vir02      klist
069300000706     C                   kfld                    vreksc
069400000706     C                   kfld                    vretfl
069500000706     C                   kfld                    vredti
069600990920     C*
069700990920     C     k01vtf01      klist
069800990920     C                   kfld                    vtftip
069900000622     C*
070000000622     C     keytbe        klist
070100000622     C                   kfld                    tbecod
070200000622     C                   kfld                    tbeke1
070300000712     C*
070400000712     C     k01vlt        klist
070500000712     C                   kfld                    vltprg
070600000712     C                   kfld                    vltksc
070700000706     C*
070800991104     C* Reperisco nome sistema.
070900991104     C                   eval      qusbprv = %size(qusec)
071000991104     C                   call      'QWCRNETA'
071100991104     C                   parm                    rneta01
071200991104     C                   parm                    rneta02
071300991104     C                   parm                    rneta03
071400991104     C                   parm                    rneta04
071500991104     C                   parm                    qusec
071600991104     C                   eval      nait = rneta01c
071700990908     C*
071800990908     C                   endsr
071900991026** ctdata mc
072000991026ABCDEFGHILMN
