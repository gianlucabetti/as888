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
001300031008      * 'E' = File/membro già allocato dalla procedura FTP/e-mail sul server
001400031008      * 'F' = Progressivo NON univoco  => ATTENZIONE, possibili errori gravi ed imprevedibili!!!
001500991026      **********************************************************************
001600991026
001700050909 TESTF*tivltnew  o    e             disk    RENAME(tivlt000:tivltn00) USROPN
001800991026     Ftivlt00f  o    e             disk
001900000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
002000031008     Ftivlt08l  if   e           k disk    RENAME(tivlt000:tivlt800)
002100031008     F                                     PREFIX(w_)
002200991105     Ftivss02l  if   e           k disk
002300990920     Ftivtf01l  if   e           k disk
002400991026     Ftivir02l  if   e           k disk
002500000707     Ftivre01l  if   e           k disk
002600000622     Ftntbe01l  if   e           k disk
002700000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002800000627     F                                     PREFIX(f_)
002900000627
003000020212     D disv          e ds
003100030526     D dvir01        e ds
003200991027     D psds           sds
003300991027     D  psjobname            244    253
003400991027     D  psuser               254    263
003500991027     D  psjobnbr             264    269s 0
003600991026     D prmksc          s                   like(virksc)
003700991026     D prmksu          s                   like(virksc)
003800991026     D prmtip          s                   like(virtip)
003900991026     D prmesito        s              1
004000000623     D prmprog         s                   like(vltprg)
004100000623     D prmsrv          s                   like(vltisv)
004200000628     D prmope          s              1
004300000713     D prmnfp          s                   like(vltfl2)
004400991026     D wrkksc          s                   like(prmksc)
004500991026     D wrkksu          s                   like(prmksu)
004600991026     D wrktip          s                   like(prmtip)
004700991026     D wrkesito        s                   like(prmesito)
004800991026     D wrkoggiiso      s               d
004900991026     D wrkoggi         s              8  0
005000991026     D wrkora          s              6  0
005100991026     D wrkmese         s              2  0
005200000623     D wrkprg          s              8  0 INZ(*zeros)
005300000711     D wrkvarie        s              1    INZ('0')
005400000712     D wrkmsg          s             80    INZ(*blanks)
005500000914     D vssok           s               n   INZ(*off)
005600051011
005700051011     D dwlprg          ds
005800051011     D  dwl_isv                       2    INZ(*all'0')
005900051207     D**dwl_tip                       2    INZ(*all'0')
006000051207     D**dwl_prg                       6    INZ(*all'0')
006100051207     D  dwl_prg                       8    INZ(*all'0')
006200000711
006300991026     D                 ds                  inz
006400991026     D wrkday                  1      2s 0
006500991026     D wrkdaychr                      2    overlay(wrkday)
006600991026     D wrkpgt          s                   like(vtfpgt)
006700991026     D wrkppt          s                   like(vtfppt)
006800000711     D wrkpgi          s                   like(vtfpgi)
006900000711     D wrkppi          s                   like(vtfppi)
007000991026     D wrkfid          s                   like(vtffid)
007100991026     D wrkmbd          s                   like(vltmbd)
007200991026     D wrkfis          s                   like(vtffis)
007300991026     D wrkmbs          s                   like(vltmbs)
007400991026     D wrkfit          s                   like(vtffit)
007500991026     D wrkmbt          s                   like(vltmbd)
007600991026     D prmfid          s                   like(wrkfid)
007700991026     D prmmbd          s                   like(wrkmbd)
007800991026     D prmfis          s                   like(wrkfis)
007900991026     D prmmbs          s                   like(wrkmbs)
008000991026     D prmfit          s                   like(wrkfit)
008100991027     D prmmbt          s                   like(wrkmbt)
008200991026     D prmpgt          s                   like(wrkpgt)
008300991026     D prmppt          s                   like(wrkppt)
008400991026     D mc              s              1    dim(12) ctdata perrcd(12)
008500991104
008600991111     D mbrd01          s            266
008700991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008800991111     D mbrd03          s              8    inz('MBRD0200')
008900991111     D mbrd04          ds
009000991111     D mbrd04a                       10                                         File
009100991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009200991111     D mbrd05          s             10                                         Membro
009300991125     D mbrd06          s              1    inz('0')
009400991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
009500991104     D rneta01         ds
009600991104     D  rneta01a                      8b 0
009700991104     D  rneta01b                      8b 0
009800991104     D  rneta01c                     24
009900991104     D rneta02         s              8b 0 inz(%size(rneta01))
010000991104     D rneta03         s              8b 0 inz(1)
010100991104     D rneta04         s             10    inz('SYSNAME')
010200991104     D nait            ds
010300991104     D  nait01                       10
010400991104     D  nait02                        1
010500991104     D  nait03                        1
010600991104     D  nait04                        8b 0
010700991104     D  sysname                       8
010800991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
010900991104
011000000622
011100000905     D*-------------------
011200000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011300000905     D*-------------------
011400000905     D  dsvlt        E DS                  extname(tivlt00f)
011500000905     D                                     prefix(s_)
011600000905
011700040817
011800040817     D*-------------------
011900050909 TESTD* Variabili d wrk
012000040817     D*-------------------
012100050909     D*  COMMAND        S            300    inz(*blanks)
012200040817
012300000905
012400000905
012500000622      /TITLE *** MAIN ***
012600000622     C*
012700000906     C                   if        prmnfp  > '01' and
012800000906     C                             prmprog > *blanks
012900000905     C                   exsr      rtvdatora
013000000905     C                   else
013100000905     C                   time                    wrkoggiiso
013200000905     C                   time                    wrkora
013300000905     C     *iso          move      wrkoggiiso    wrkoggi
013400000905     C                   endif
013500030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
013600000328     C* Inizializzo variabili "wrk".
013700000328     C                   exsr      inzwrk
013800991026     C* Controllo parametri ricevuti.
013900991026     C                   exsr      chkprm
014000000626     C* Reperisco informazioni file.
014100000706     C                   exsr      rtvleg
014200000706     C* Alla fine...
014300000706     C                   exsr      uscita
014400000626
014500000626
014600000905
014700000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
014800000905     C*
014900000905     C     rtvdatora     begsr
015000000905     C*
015100000905     C/EXEC SQL
015200000905     C+ declare C1 cursor for select *
015300000905     C+ from tivlt00f where
015400000905     C+ vltprg = :prmprog
015500000905     C+ for fetch only
015600000905     C/END-EXEC
015700000905     C
015800000905     C/EXEC SQL
015900000905     C+ open C1
016000000905     C/END-EXEC
016100000905     C
016200000905     C/EXEC SQL
016300000905     C+ Fetch C1 into :dsvlt
016400000905     C/END-EXEC
016500000905     C*
016600000906     C                   if        sqlcod = *zeros     and
016700000906     C                             s_vltprg  = prmprog and
016800000906     C                             s_vltdat  > *zeros  and
016900000905     C                             s_vltora  > *zeros
017000000905     C                   z-add     s_vltdat      wrkoggi
017100000905     C                   z-add     s_vltora      wrkora
017200000905     C                   else
017300000905     C                   time                    wrkoggiiso
017400000905     C                   time                    wrkora
017500000905     C     *iso          move      wrkoggiiso    wrkoggi
017600000905     C                   endif
017700000905     C
017800000905     C/EXEC SQL
017900000905     C+ close C1
018000000905     C/END-EXEC
018100000905     C
018200000905     C*
018300000905     C                   endsr
018400000905
018500000905
018600000706
018700000706      /TITLE Determino eventuali legami cliente/tipi file
018800000706     C     rtvleg        begsr
018900000706     C*
019000000706     C                   eval      virksc = wrkksu
019100000706     C                   eval      virtip = wrktip
019200000706     C                   eval      virdti = wrkoggi
019300000707     C                   eval      vtftip = wrktip
019400000706     C* Reperisco informazioni file.
019500000706     C                   exsr      rtvtip
019600000706     C*
019700000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
019800000713     C                   if        wrkesito = '0'     and
019900000713     C                             prmprog  = *blanks and
020000000713     C                             prmope   = *blanks
020100000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
020200000711     C                   movel     '1'           wrkvarie
020300000706     C*
020400000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
020500000707     C     k03vir02      setll     tivre01l
020600000707     C                   if        %found(tivre01l)
020700000707     C     k03vir02      reade     tivre01l
020800000706     C                   dow       not %eof
020900000706     C                   eval      virksc = wrkksu
021000000706     C                   eval      virtip = vretfl
021100000706     C                   eval      virdti = wrkoggi
021200000707     C                   eval      vtftip = vretfl
021300000712     C                   eval      wrktip = vretfl
021400000706     C* Reperisco informazioni file.
021500000706     C                   exsr      rtvtip
021600000712     C                   eval      virtip = vretip
021700000707     C     k03vir02      reade     tivre01l
021800000706     C                   enddo
021900000706     C                   endif
022000000706     C*
022100000706     C                   endif
022200031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
022300031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
022400031008     C                   if        chkPRG <> '1'
022500030512     C                   if        chkFTP <> '1'
022600000711     C                   exsr      exepgm
022700030512     C                   endif
022800031008     C                   endif
022900000706     C*
023000000706     C                   endsr
023100000706
023200000706
023300000706
023400000706      /TITLE Reperisco tipo file.
023500000706     C     rtvtip        begsr
023600000706     C* Reperisco il tipo file.
023700000706     C     k01vtf01      chain     tivtf01l
023800000706     C* Se il tipo file non esiste
023900000706     C* oppure non è valido per il download.
024000000706     C                   if        not %found
024100000706     C                             or vtfud <> 'D'
024200031008     C*
024300031008     C                   exsr      inztivlt
024400031008     C                   eval      vltsts = '2'
024500031008     C                   eval      vltmsg =
024600031008     C                               %trim(sysname)
024700031008     C                             + '/'
024800031008     C                             + %trim(psjobname)
024900031008     C                             + '/'
025000031008     C                             + %trim(psuser)
025100031008     C                             + '/'
025200031008     C                             + %trim(%editc(psjobnbr:'Z'))
025300031008     C                             + ' Tipo file inesistente.'
025400031008     C                   exsr      wrttivlt
025500031008     C                   eval      wrkesito = 'B'
025600031008     C*
025700000721     C                   else
025800000706     C* Reperisco il tipo file del cliente.
025900000706     C     k03vir02      setgt     tivir02l
026000000706     C     k02vir02      readpe    tivir02l
026100000706     C* Tipo file non previsto per il cliente
026200000706     C* oppure scaduto.
026300000731     C                   if        %eof
026400000706     C                             or virdtf < wrkoggi
026500000706     C                             or virdti > wrkoggi
026600000706     C*
026700000731     C                   exsr      inztivltp
026800000706     C*
026900000706     C                   eval      vltsts = '2'
027000000706     C                   eval      vltmsg =
027100000706     C                               %trim(sysname)
027200000706     C                             + '/'
027300000706     C                             + %trim(psjobname)
027400000706     C                             + '/'
027500000706     C                             + %trim(psuser)
027600000706     C                             + '/'
027700000706     C                             + %trim(%editc(psjobnbr:'Z'))
027800000706     C                             + ' Tipo file non previsto per il cliente.'
027900000706     C                   exsr      wrttivlt
028000000706     C                   eval      wrkesito = 'C'
028100000706     C*
028200000706     C                   else
028300000706     C*
028400000706     C* Imposto i default.
028500000706     C                   if        virpgt <> *blanks
028600000706     C                   eval      wrkpgt = virpgt
028700000706     C                   eval      wrkppt = virppt
028800000706     C                   else
028900000706     C                   eval      wrkpgt = vtfpgt
029000000706     C                   eval      wrkppt = vtfppt
029100000706     C                   endif
029200000712     C*
029300000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
029400000711     C                   if        wrkvarie = '0'                               <===================
029500000711     C                   if        virpgi <> *blanks                            <===================
029600000711     C                   eval      wrkpgi = virpgi                              <===================
029700000711     C                   eval      wrkppi = virppi                              <===================
029800000711     C                   else                                                   <===================
029900000711     C                   eval      wrkpgi = vtfpgi                              <===================
030000000711     C                   eval      wrkppi = vtfppi                              <===================
030100000711     C                   endif                                                  <===================
030200000711     C                   endif                                                  <===================
030300000706     C* File da tradurre.
030400000706     C                   if        virfid <> *blanks
030500000706     C                   eval      wrkfid = virfid
030600000706     C                   else
030700000706     C                   eval      wrkfid = vtffid
030800000706     C                   endif
030900000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
031000000706     C* File storico.
031100000706     C                   if        virfis <> *blanks
031200000706     C                   eval      wrkfis = virfis
031300000706     C                   else
031400000706     C                   eval      wrkfis = vtffis
031500000706     C                   endif
031600000706     C                   extrct    wrkoggiiso:*m wrkmese
031700000706     C                   extrct    wrkoggiiso:*d wrkday
031800000706     C                   eval      wrkmbs =
031900000706     C                             mc(wrkmese)
032000000706     C                             + wrkdaychr
032100000706     C                             + %subst(wrkksu:2:7)
032200030519     C*
032300030519     C                   eval      vssisv = virfi1
032400030519     C* Valorizzo subito il progressivo applicazione.
032500030519     C                   exsr      calprog
032600000706     C* File tradotto.
032700000706     C                   if        virfit <> *blanks
032800000706     C                   eval      wrkfit = virfit
032900000706     C                   else
033000000706     C                   eval      wrkfit = vtffit
033100000706     C                   endif
033200030526     C* Tipo nome membro tradotto
033300030526     C                   eval      dvir01 = virpth
033400030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
033500030526     C                   if        §VIR01TM = 'P'
033600030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
033700030526 xxx C***                eval      wrkmbs = dwlprg
033800030519     C                   else
033900030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
034000030519     C                   endif
034100000706     C* Controllo abilitazione al servizio.
034200000706     C                   exsr      chkvss
034300000706     C                   endif
034400000706     C                   endif
034500000706     C*
034600000706     C                   endsr
034700000706
034800000711
034900000711
035000000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
035100000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
035200000711     C*
035300000711     C     exepgm        begsr
035400000711     C* Lancio il pgm in variabile con relativi parametri
035500000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
035600000711    C                   if        wrkpgi <> *blanks
035700000712     C* Chiamo il pgm post traduzione.
035800000712     C                   call(e)   'TIS7T9C1'
035900000712     C                   parm      wrkfid        prmfid
036000000712     C                   parm      wrkmbd        prmmbd
036100000712     C                   parm      wrkfis        prmfis
036200000712     C                   parm      wrkmbs        prmmbs
036300031106     C                   parm      wrkfit        prmfit
036400031106     C                   parm      wrkmbt        prmmbt
036500000712     C                   parm                    wrkpgi
036600000712     C                   parm                    wrkppi
036700000712     C                   parm      *blanks       prmesito
036800000712     C* Se la call è finita in errore
036900000712     C* o il lancio del PGM ha avuto degli errori.
037000000712     C                   if        %error
037100000712     C                             or prmesito <> '0'
037200000712     C     k01vlt        setll     tivlt07l
037300000712     C                   if        %found(tivlt07l)
037400000712     C     k01vlt        reade     tivlt07l
037500000712     C                   dow       not %eof
037600000712     C                   if        vltsqz = *zeros and
037700000712     C                             (vltsts = '0' or vltsts = '1')
037800000712     C                   eval      vltsts = '2'
037900000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
038000000712     C                   eval      vltmsg =
038100000712     C                               %trim(sysname)
038200000712     C                             + '/'
038300000712     C                             + %trim(psjobname)
038400000712     C                             + '/'
038500000712     C                             + %trim(psuser)
038600000712     C                             + '/'
038700000712     C                             + %trim(%editc(psjobnbr:'Z'))
038800000712     C                             + %trim(wrkmsg)
038900000712     C                   update    tivlt700
039000000712     C                   endif
039100000712     C     k01vlt        reade     tivlt07l
039200000712     C                   enddo
039300000712     C                   endif
039400000712     C                   endif
039500000712     C*
039600000711     C                   endif
039700000711     C*
039800000711     C                   endsr
039900000711
040000000711
040100000626
040200000626      /TITLE Valorizzazione Progressivo Applicazione
040300000626     C     calprog       begsr
040400000626     C*
040500000706     C                   if        dwlprg = *all'0'
040600000627     C                   read(e)   tis7prgf
040700000627     C                   if        not %error
040800000627     C                   eval      dwlprg = f_tis7prgf
040900000627     C*
041000000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
041100000719     C                   if        prmsrv <> *blanks
041200000719     C                   eval      vssisv = prmsrv
041300000719     C                   endif
041400000626     C*
041500000719     C                   if        prmprog = *blanks
041600051011     C                   move(p)   dwl_prg       wrkprg
041700000626     C                   add       1             wrkprg
041800051011     C                   move(p)   wrkprg        dwl_prg
041900051011     C                   movel     vssisv        dwl_isv
042000051207     C*                  if        wrktip <> *blanks
042100051207     C*                  movel     wrktip        dwl_tip
042200051207     C*                  else
042300051207     C*                  movel     'XX'          dwl_tip
042400051207     C*                  endif
042500040123     C                   eval      f_tis7prgf = dwlprg
042600040123     C                   update    tis7prg0
042700000626     C                   else
042800040123     C                   unlock(e) tis7prgf
042900000626     C                   eval      dwlprg = prmprog
043000000626     C                   endif
043100000627     C*
043200000627     C                   endif
043300000706     C                   endif
043400000626     C*
043500000626     C                   endsr
043600991104
043700000706
043800000706
043900000706
044000991104      /TITLE Controllo abilitazione servizio.
044100991104     C     chkvss        begsr
044200991104     C*
044300000914     C                   eval      vssok = *off
044400000914     C*
044500991105     C                   eval      vssksu = wrkksu
044600000914     C     k02vss02      setll     tivss02l
044700000914     C     k02vss02      reade     tivss02l
044800000914     C                   dow       not %eof
044900000914     C                   if        vssdde <= wrkoggi and
045000000914     C                             vssdsc >= wrkoggi and
045100000914     C                             vssvat <> 'A'     and
045200000914     C                             vssvat <> 'S'
045300000914     C                   eval      vssok = *on
045400000914     C                   leave
045500000914     C                   endif
045600000914     C     k02vss02      reade     tivss02l
045700000914     C                   enddo
045800000914     C*
045900000914     C                   if        vssok = *off
046000991104     C*
046100000622     C                   exsr      inztivlt
046200000329     C                   eval      vltsts = '2'
046300991104     C                   eval      vltmsg =
046400991104     C                               %trim(sysname)
046500991104     C                             + '/'
046600991104     C                             + %trim(psjobname)
046700991104     C                             + '/'
046800991104     C                             + %trim(psuser)
046900991104     C                             + '/'
047000991104     C                             + %trim(%editc(psjobnbr:'Z'))
047100991104     C                             + ' Cliente non abilitato al servizio.'
047200000622     C                   exsr      wrttivlt
047300000622     C                   eval      wrkesito = 'D'
047400031008     C*
047500000622     C                   else
047600031008     C*
047700031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
047800031008     C* del progressivo rispetto a file/membro e cliente
047900031008     C                   movel     '0'           chkPRG            1
048000031008     C     k08vlt        setll     tivlt08l
048100031008     C                   if        %equal(tivlt08l)
048200031008     C     k08vlt        reade     tivlt08l
048300031008     C                   dow       not %eof(tivlt08l)
048400031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
048500031008     C                   if        w_vltksc <> wrkksc
048600031008     C                   movel     '1'           chkPRG
048700031008     C                   endif
048800031008     C     k08vlt        reade     tivlt08l
048900031008     C                   enddo
049000031008     C                   endif
049100031008     C*
049200000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
049300031008     C                   if        chkPRG = '0'
049400000706     C                   exsr      traduci
049500031008     C                   else
049600031008     C*
049700031008     C                   exsr      inztivlt
049800031008     C                   eval      vltsts = '2'
049900031008     C                   eval      vltmsg =
050000031008     C                               %trim(sysname)
050100031008     C                             + '/'
050200031008     C                             + %trim(psjobname)
050300031008     C                             + '/'
050400031008     C                             + %trim(psuser)
050500031008     C                             + '/'
050600031008     C                             + %trim(%editc(psjobnbr:'Z'))
050700031008     C                             + ' Progressivo/membro NON univoco!!!'
050800031008     C                   exsr      wrttivlt
050900031008     C                   eval      wrkesito = 'F'
051000031008     C                   exsr      sndeml
051100031008     C                   exsr      uscita
051200031008     C                   endif
051300991104     C*
051400991104     C                   endif
051500991104     C*
051600991104     C                   endsr
051700000622
051800000622
051900000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
052000000622     C     rtvtia        begsr
052100000622     C*
052200000622     C                   eval      tbecod = 'ISV'
052300000731     C                   eval      tbeke1 = virfi1
052400000622     C     keytbe        chain     tntbe01l
052500000622     C                   if        %found(tntbe01l)
052600020212     C                   movel     tbeuni        disv
052700000622     C                   endif
052800000622     C*
052900000622     C                   endsr
053000000706
053100000706
053200000706
053300000706      /TITLE Traduzione.
053400000706     C     traduci       begsr
053500031105     C*
053600031105     C* Reperisco informazioni membro da tradurre.
053700031105     C                   eval      mbrd04a = wrkfid
053800031105     C                   eval      mbrd05  = wrkmbd
053900031105     C                   exsr      rtvmbrd
054000031105     C* Controllo se il membro da tradurre ha dei record.
054100031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
054200031105     C                   if        (qusei=*blanks and
054300060616     C                             qusnbrcr > 0) or prmnfp > '01'
054400060616     C                             or %trim(wrkfid) = '*'
054500030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
054600030512     C                   call      'TIS7T9R2'
054700031008     C                   parm                    wrkfit
054800031008     C                   parm                    wrkmbt
054900030512     C                   parm      '0'           chkFTP            1
055000031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
055100031105     C                   if        chkFTP = '0'
055200000706     C*
055300000706     C                   exsr      inztivlt
055400000706     C* Chiamo il pgm traduttore.
055500060616     C                   if        %trim(wrkfid) = '*' AND
055600060616     C                             %trim(wrkfis) = '*'
055700060616     C                   call(e)   'TIS7T9C3'
055800060616     C                   parm      wrkmbd        prmmbd
055900060616     C                   parm      wrkfit        prmfit
056000060616     C                   parm      wrkmbt        prmmbt
056100060616     C                   parm      wrkpgt        prmpgt
056200060616     C                   parm      wrkppt        prmppt
056300060616     C                   parm      *blanks       prmesito
056400060616     C                   else
056500000706     C                   call(e)   'TIS7T9C'
056600000706     C                   parm      wrkfid        prmfid
056700000706     C                   parm      wrkmbd        prmmbd
056800000706     C                   parm      wrkfis        prmfis
056900000706     C                   parm      wrkmbs        prmmbs
057000000706     C                   parm      wrkfit        prmfit
057100000706     C                   parm      wrkmbt        prmmbt
057200000706     C                   parm      wrkpgt        prmpgt
057300000706     C                   parm      wrkppt        prmppt
057400000706     C                   parm      *blanks       prmesito
057500060616     C                   endif
057600000706     C* Se la call è finita in errore
057700000706     C* o la traduzione ha avuto degli errori.
057800060320     C                   if        %error
057900060320     C                             or (prmesito <> '0' and
058000060320     C                                 prmesito <> 'Y')
058100000706     C                   eval      vltsts = '2'
058200000706     C                   eval      vltmsg =
058300000706     C                               %trim(sysname)
058400000706     C                             + '/'
058500000706     C                             + %trim(psjobname)
058600000706     C                             + '/'
058700000706     C                             + %trim(psuser)
058800000706     C                             + '/'
058900000706     C                             + %trim(%editc(psjobnbr:'Z'))
059000000706     C                             + ' Errori nella traduzione.'
059100000706     C                   else
059200060320     C                   if        prmesito = 'Y'
059300060320     C                   eval      wrkesito = prmesito
059400060320     C                   else
059500000706     C                   eval      wrkesito = '0'
059600060320     C                   endif
059700000706     C                   endif
059800000706     C*
059900000706     C                   exsr      wrttivlt
060000031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
060100031105     C                   else
060200030512     C                   exsr      inztivlt
060300030512     C                   eval      vltsts = '2'
060400030512     C                   eval      vltmsg =
060500030512     C                               %trim(sysname)
060600030512     C                             + '/'
060700030512     C                             + %trim(psjobname)
060800030512     C                             + '/'
060900030512     C                             + %trim(psuser)
061000030512     C                             + '/'
061100030512     C                             + %trim(%editc(psjobnbr:'Z'))
061200030512     C                             + ' Cliente/TipoFile in uso da server FTP'
061300030512     C                   exsr      wrttivlt
061400030512     C                   eval      wrkesito = 'E'
061500030512     C                   endif
061600031105     C* NO record da tradurre
061700031105     C                   endif
061800000706     C*
061900000706     C                   endsr
062000000328
062100000706
062200000706
062300000328      /TITLE Inizializzo variabili "wrk".
062400000328     C     inzwrk        begsr
062500000328     C*
062600000328     C                   clear                   wrkfit
062700000328     C                   clear                   wrkmbt
062800000328     C                   clear                   wrkpgi
062900000328     C                   clear                   wrkppi
063000000328     C                   clear                   wrkfis
063100000328     C                   clear                   wrkmbs
063200000328     C*
063300000328     C                   endsr
063400991026
063500000706
063600000706
063700991026      /TITLE Inizializzo TIVLT00F.
063800991026     C     inztivlt      begsr
063900000804     C*
064000000804     C                   exsr      rtvtia
064100991026     C*
064200000328     C                   clear                   tivlt000
064300991026     C                   eval      vltksc = wrkksc
064400991026     C                   eval      vltksu = wrkksu
064500000721     C                   eval      vlttip = wrktip
064600991026     C                   eval      vltfld = wrkfit
064700991026     C                   eval      vltmbd = wrkmbt
064800991026     C                   eval      vltpgi = wrkpgi
064900991026     C                   eval      vltppi = wrkppi
065000020212     C                   if        §isvproc = 'F'
065100000804     C                   eval      vltsnd = 'D'
065200000804     C                   else
065300991026     C                   eval      vltsnd = vtfsnd
065400000804     C                   endif
065500991026     C                   eval      vltfls = wrkfis
065600991026     C                   eval      vltmbs = wrkmbs
065700991026     C                   eval      vltdat = wrkoggi
065800991115     C                   eval      vltora = wrkora
065900991026     C                   eval      vltsts = '0'
066000991027     C                   eval      vltmsg =
066100991104     C                               %trim(sysname)
066200991104     C                             + '/'
066300991104     C                             + %trim(psjobname)
066400991027     C                             + '/'
066500991027     C                             + %trim(psuser)
066600991027     C                             + '/'
066700991027     C                             + %trim(%editc(psjobnbr:'Z'))
066800000328     C                   z-add     *zeros        vltsqz
066900000731     C*
067000991026     C                   endsr
067100991026
067200000706
067300000731
067400000731      /TITLE Inizializzo parzialmente TIVLT00F.
067500000731     C     inztivltp     begsr
067600000731     C*
067700000731     C                   clear                   tivlt000
067800000731     C                   eval      vltksc = wrkksc
067900000731     C                   eval      vltksu = wrkksu
068000000731     C                   eval      vlttip = wrktip
068100000731     C                   eval      vltpgi = wrkpgi
068200000731     C                   eval      vltppi = wrkppi
068300000731     C                   eval      vltsnd = vtfsnd
068400000731     C                   eval      vltdat = wrkoggi
068500000731     C                   eval      vltora = wrkora
068600000731     C                   eval      vltsts = '0'
068700000731     C                   eval      vltmsg =
068800000731     C                               %trim(sysname)
068900000731     C                             + '/'
069000000731     C                             + %trim(psjobname)
069100000731     C                             + '/'
069200000731     C                             + %trim(psuser)
069300000731     C                             + '/'
069400000731     C                             + %trim(%editc(psjobnbr:'Z'))
069500000731     C                   z-add     *zeros        vltsqz
069600000731     C*
069700000731     C                   exsr      rtvtia
069800000731     C*
069900000731     C                   endsr
070000000731
070100000731
070200000706
070300991026      /TITLE Scrittura TIVLT00F.
070400000731     C     wrttivlt      begsr
070500991026     C*
070600000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
070700000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
070800000731     C                   eval      vltisv = virfi1
070900020212     C                   eval      vlttia = §isvproc
071000000622     C                   eval      vltprg = dwlprg
071100000713     C                   eval      vltfl2 = prmnfp
071200991026     C                   write     tivlt000
071300040817     C*
071400050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
071500040817     C* procedura scambio dati
071600040817     C* ...quindi eseguo prima d tutto l'override
071700050909  "  C*
071800050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
071900050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
072000050909     C*                  eval      LUNG = %len(%trim(COMMAND))
072100050909  "  C*                  call(e)   'QCMDEXC'
072200050909     C*                  parm                    COMMAND
072300050909     C*                  parm                    LUNG             15 5
072400040817     C*
072500050909  "  C*                  if        not %error
072600050909     C*                  if        not %open(tivltnew)
072700050909     C*                  open(e)   tivltnew
072800050909     C*                  endif
072900050909  "  C*                  if        not %error
073000050909     C*                  write     tivltn00
073100050909     C*                  endif
073200050909     C*                  if        %open(tivltnew)
073300050909  "  C*                  close(e)  tivltnew
073400050909     C*                  endif
073500050909 TESTC*                  endif
073600991026     C*
073700991026     C                   endsr
073800991026
073900000706
074000000706
074100991026      /TITLE Operazioni finali.
074200991026     C     uscita        begsr
074300991026     C*
074400000627     C                   seton                                        lr
074500991026     C*
074600991026     C                   endsr
074700991026
074800000706
074900000706
075000991026      /TITLE Controllo formale parametri ricevuti.
075100991026     C     chkprm        begsr
075200991026     C*
075300991104     C                   if        wrkksu = *blanks
075400031008     C*
075500031008     C                   exsr      inztivlt
075600031008     C                   eval      vltsts = '2'
075700031008     C                   eval      vltmsg =
075800031008     C                               %trim(sysname)
075900031008     C                             + '/'
076000031008     C                             + %trim(psjobname)
076100031008     C                             + '/'
076200031008     C                             + %trim(psuser)
076300031008     C                             + '/'
076400031008     C                             + %trim(%editc(psjobnbr:'Z'))
076500031008     C                             + ' Parametro codice cliente non passato.'
076600031008     C                   exsr      wrttivlt
076700031008     C                   eval      wrkesito = 'A'
076800031008     C*
076900991026     C                   exsr      uscita
077000991026     C                   endif
077100991026     C*
077200991026     C                   if        wrktip = *blanks
077300031008     C*
077400031008     C                   exsr      inztivlt
077500031008     C                   eval      vltsts = '2'
077600031008     C                   eval      vltmsg =
077700031008     C                               %trim(sysname)
077800031008     C                             + '/'
077900031008     C                             + %trim(psjobname)
078000031008     C                             + '/'
078100031008     C                             + %trim(psuser)
078200031008     C                             + '/'
078300031008     C                             + %trim(%editc(psjobnbr:'Z'))
078400031008     C                             + ' Parametro tipo file non passato.'
078500031008     C                   exsr      wrttivlt
078600031008     C                   eval      wrkesito = 'B'
078700031008     C*
078800991026     C                   exsr      uscita
078900991026     C                   endif
079000991026     C*
079100991026     C                   clear                   wrkesito
079200991026     C*
079300991026     C                   endsr
079400991111
079500000706
079600000706
079700991111      /TITLE Reperisco informazioni membro.
079800991111     C     rtvmbrd       begsr
079900991111     C* Pulisco e imposto la DS degli errori.
080000000124     C                   clear                   qusec
080100991111     C                   eval      qusbprv = %size(qusec)
080200991111     C*
080300991111     C                   call      'QUSRMBRD'
080400991111     C     qusm0200      parm                    mbrd01
080500991111     C                   parm                    mbrd02
080600991111     C                   parm                    mbrd03
080700991111     C                   parm                    mbrd04
080800991111     C                   parm                    mbrd05
080900991111     C                   parm                    mbrd06
081000991111     C                   parm                    qusec
081100991111     C*
081200991111     C                   endsr
081300031008
081400031008
081500031008
081600031008
081700031008      /TITLE Compongo il testo e spedisco una e-m@ail
081800031008     C     sndeml        begsr
081900031008     C*
082000031008     C* Inizializzo variabili
082100031008     C                   movel     *blanks       wrkEml          253
082200031008     C                   movel     *blanks       wrkEmlMsg      5000
082300031008     C                   movel     *blanks       wrkEmlOgg        44
082400031008     C* Valorizzo i campi della e-m@ail
082500120305     C                   eval      wrkEml='cedalert@brt.it'
082600031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
082700031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
082800031008     C                             'SU AS/400'                         +':/N'+
082900031008     C                             ' '                                 +':/N'+
083000031008     C                             'RIFERIMENTI'                       +':/N'+
083100031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
083200031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
083300031008     C                             'Cliente:'+vltksc                   +':/N'+
083400031008     C                             'TipoFile:'+vlttip                  +':/N'+
083500031008     C                             ' '                                 +':/N'+
083600031008     C                             'MESSAGGIO'                         +':/N'+
083700031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
083800031008     C*
083900031008     C                   call(e)   'TIS701C'
084000031008     C                   parm                    wrkEml
084100031008     C                   parm                    wrkEmlOgg
084200031008     C                   parm                    wrkEmlMsg
084300031008     C*
084400031008     C                   endsr
084500990910
084600000706
084700000706
084800990908      /TITLE Operazioni iniziali.
084900990908     C     *inzsr        begsr
085000990908     C*
085100990908     C     *entry        plist
085200991026     C     wrkksc        parm                    prmksc
085300991026     C     wrkksu        parm      wrkksu        prmksu
085400991026     C     wrktip        parm                    prmtip
085500991026     C                   parm      wrkesito      prmesito
085600000623     C                   parm                    prmprog
085700000623     C                   parm                    prmsrv
085800000628     C                   parm                    prmope
085900000713     C                   parm                    prmnfp
086000991104     C*
086100991105     C     k02vss02      klist
086200991105     C                   kfld                    vssksu
086300991104     C                   kfld                    vssisv
086400991026     C*
086500991026     C     k03vir02      klist
086600991026     C                   kfld                    virksc
086700991026     C                   kfld                    virtip
086800991026     C                   kfld                    virdti
086900991026     C*
087000991026     C     k02vir02      klist
087100991026     C                   kfld                    virksc
087200991026     C                   kfld                    virtip
087300000706     C*
087400000706     C     k04vir02      klist
087500000706     C                   kfld                    vreksc
087600000706     C                   kfld                    vretfl
087700000706     C                   kfld                    vredti
087800990920     C*
087900990920     C     k01vtf01      klist
088000990920     C                   kfld                    vtftip
088100000622     C*
088200000622     C     keytbe        klist
088300000622     C                   kfld                    tbecod
088400000622     C                   kfld                    tbeke1
088500000712     C*
088600000712     C     k01vlt        klist
088700000712     C                   kfld                    vltprg
088800000712     C                   kfld                    vltksc
088900031008     C*
089000031008     C     k08vlt        klist
089100031008     C                   kfld                    wrkfit
089200031008     C                   kfld                    wrkmbt
089300000706     C*
089400991104     C* Reperisco nome sistema.
089500991104     C                   eval      qusbprv = %size(qusec)
089600991104     C                   call      'QWCRNETA'
089700991104     C                   parm                    rneta01
089800991104     C                   parm                    rneta02
089900991104     C                   parm                    rneta03
090000991104     C                   parm                    rneta04
090100991104     C                   parm                    qusec
090200991104     C                   eval      nait = rneta01c
090300990908     C*
090400990908     C                   endsr
090500991026** ctdata mc
090600991026ABCDEFGHILMN
