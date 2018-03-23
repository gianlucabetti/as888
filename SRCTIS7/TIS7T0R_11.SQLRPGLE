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
004400070207     D prmout          s             10
004500991026     D wrkksc          s                   like(prmksc)
004600991026     D wrkksu          s                   like(prmksu)
004700991026     D wrktip          s                   like(prmtip)
004800991026     D wrkesito        s                   like(prmesito)
004900991026     D wrkoggiiso      s               d
005000991026     D wrkoggi         s              8  0
005100991026     D wrkora          s              6  0
005200991026     D wrkmese         s              2  0
005300000623     D wrkprg          s              8  0 INZ(*zeros)
005400000711     D wrkvarie        s              1    INZ('0')
005500000712     D wrkmsg          s             80    INZ(*blanks)
005600000914     D vssok           s               n   INZ(*off)
005700051011
005800051011     D dwlprg          ds
005900051011     D  dwl_isv                       2    INZ(*all'0')
006000051207     D**dwl_tip                       2    INZ(*all'0')
006100051207     D**dwl_prg                       6    INZ(*all'0')
006200051207     D  dwl_prg                       8    INZ(*all'0')
006300000711
006400991026     D                 ds                  inz
006500991026     D wrkday                  1      2s 0
006600991026     D wrkdaychr                      2    overlay(wrkday)
006700991026     D wrkpgt          s                   like(vtfpgt)
006800991026     D wrkppt          s                   like(vtfppt)
006900000711     D wrkpgi          s                   like(vtfpgi)
007000000711     D wrkppi          s                   like(vtfppi)
007100991026     D wrkfid          s                   like(vtffid)
007200991026     D wrkmbd          s                   like(vltmbd)
007300991026     D wrkfis          s                   like(vtffis)
007400991026     D wrkmbs          s                   like(vltmbs)
007500991026     D wrkfit          s                   like(vtffit)
007600991026     D wrkmbt          s                   like(vltmbd)
007700991026     D prmfid          s                   like(wrkfid)
007800991026     D prmmbd          s                   like(wrkmbd)
007900991026     D prmfis          s                   like(wrkfis)
008000991026     D prmmbs          s                   like(wrkmbs)
008100991026     D prmfit          s                   like(wrkfit)
008200991027     D prmmbt          s                   like(wrkmbt)
008300991026     D prmpgt          s                   like(wrkpgt)
008400991026     D prmppt          s                   like(wrkppt)
008500991026     D mc              s              1    dim(12) ctdata perrcd(12)
008600991104
008700991111     D mbrd01          s            266
008800991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008900991111     D mbrd03          s              8    inz('MBRD0200')
009000991111     D mbrd04          ds
009100991111     D mbrd04a                       10                                         File
009200991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009300991111     D mbrd05          s             10                                         Membro
009400991125     D mbrd06          s              1    inz('0')
009500991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
009600991104     D rneta01         ds
009700991104     D  rneta01a                      8b 0
009800991104     D  rneta01b                      8b 0
009900991104     D  rneta01c                     24
010000991104     D rneta02         s              8b 0 inz(%size(rneta01))
010100991104     D rneta03         s              8b 0 inz(1)
010200991104     D rneta04         s             10    inz('SYSNAME')
010300991104     D nait            ds
010400991104     D  nait01                       10
010500991104     D  nait02                        1
010600991104     D  nait03                        1
010700991104     D  nait04                        8b 0
010800991104     D  sysname                       8
010900991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
011000991104
011100000622
011200000905     D*-------------------
011300000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011400000905     D*-------------------
011500000905     D  dsvlt        E DS                  extname(tivlt00f)
011600000905     D                                     prefix(s_)
011700000905
011800040817
011900040817     D*-------------------
012000050909 TESTD* Variabili d wrk
012100040817     D*-------------------
012200050909     D*  COMMAND        S            300    inz(*blanks)
012300040817
012400000905
012500000905
012600000622      /TITLE *** MAIN ***
012700090414     C*
012800090414     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
012900090414     C
013000090414     C/EXEC SQL
013100090414     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013200090414     C/END-EXEC
013300000622     C*
013400000906     C                   if        prmnfp  > '01' and
013500000906     C                             prmprog > *blanks
013600000905     C                   exsr      rtvdatora
013700000905     C                   else
013800000905     C                   time                    wrkoggiiso
013900000905     C                   time                    wrkora
014000000905     C     *iso          move      wrkoggiiso    wrkoggi
014100000905     C                   endif
014200030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
014300000328     C* Inizializzo variabili "wrk".
014400000328     C                   exsr      inzwrk
014500991026     C* Controllo parametri ricevuti.
014600991026     C                   exsr      chkprm
014700000626     C* Reperisco informazioni file.
014800000706     C                   exsr      rtvleg
014900000706     C* Alla fine...
015000000706     C                   exsr      uscita
015100000626
015200000626
015300000905
015400000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
015500000905     C*
015600000905     C     rtvdatora     begsr
015700000905     C*
015800000905     C/EXEC SQL
015900000905     C+ declare C1 cursor for select *
016000000905     C+ from tivlt00f where
016100000905     C+ vltprg = :prmprog
016200000905     C+ for fetch only
016300000905     C/END-EXEC
016400000905     C
016500000905     C/EXEC SQL
016600000905     C+ open C1
016700000905     C/END-EXEC
016800000905     C
016900000905     C/EXEC SQL
017000000905     C+ Fetch C1 into :dsvlt
017100000905     C/END-EXEC
017200000905     C*
017300000906     C                   if        sqlcod = *zeros     and
017400000906     C                             s_vltprg  = prmprog and
017500000906     C                             s_vltdat  > *zeros  and
017600000905     C                             s_vltora  > *zeros
017700000905     C                   z-add     s_vltdat      wrkoggi
017800000905     C                   z-add     s_vltora      wrkora
017900000905     C                   else
018000000905     C                   time                    wrkoggiiso
018100000905     C                   time                    wrkora
018200000905     C     *iso          move      wrkoggiiso    wrkoggi
018300000905     C                   endif
018400000905     C
018500000905     C/EXEC SQL
018600000905     C+ close C1
018700000905     C/END-EXEC
018800000905     C
018900000905     C*
019000000905     C                   endsr
019100000905
019200000905
019300000706
019400000706      /TITLE Determino eventuali legami cliente/tipi file
019500000706     C     rtvleg        begsr
019600000706     C*
019700000706     C                   eval      virksc = wrkksu
019800000706     C                   eval      virtip = wrktip
019900000706     C                   eval      virdti = wrkoggi
020000000707     C                   eval      vtftip = wrktip
020100000706     C* Reperisco informazioni file.
020200000706     C                   exsr      rtvtip
020300000706     C*
020400000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
020500000713     C                   if        wrkesito = '0'     and
020600000713     C                             prmprog  = *blanks and
020700000713     C                             prmope   = *blanks
020800000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
020900000711     C                   movel     '1'           wrkvarie
021000000706     C*
021100000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
021200000707     C     k03vir02      setll     tivre01l
021300000707     C                   if        %found(tivre01l)
021400000707     C     k03vir02      reade     tivre01l
021500000706     C                   dow       not %eof
021600000706     C                   eval      virksc = wrkksu
021700000706     C                   eval      virtip = vretfl
021800000706     C                   eval      virdti = wrkoggi
021900000707     C                   eval      vtftip = vretfl
022000000712     C                   eval      wrktip = vretfl
022100000706     C* Reperisco informazioni file.
022200000706     C                   exsr      rtvtip
022300000712     C                   eval      virtip = vretip
022400000707     C     k03vir02      reade     tivre01l
022500000706     C                   enddo
022600000706     C                   endif
022700000706     C*
022800000706     C                   endif
022900031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
023000031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
023100031008     C                   if        chkPRG <> '1'
023200030512     C                   if        chkFTP <> '1'
023300000711     C                   exsr      exepgm
023400030512     C                   endif
023500031008     C                   endif
023600000706     C*
023700000706     C                   endsr
023800000706
023900000706
024000000706
024100000706      /TITLE Reperisco tipo file.
024200000706     C     rtvtip        begsr
024300000706     C* Reperisco il tipo file.
024400000706     C     k01vtf01      chain     tivtf01l
024500000706     C* Se il tipo file non esiste
024600000706     C* oppure non è valido per il download.
024700000706     C                   if        not %found
024800000706     C                             or vtfud <> 'D'
024900031008     C*
025000031008     C                   exsr      inztivlt
025100031008     C                   eval      vltsts = '2'
025200031008     C                   eval      vltmsg =
025300031008     C                               %trim(sysname)
025400031008     C                             + '/'
025500031008     C                             + %trim(psjobname)
025600031008     C                             + '/'
025700031008     C                             + %trim(psuser)
025800031008     C                             + '/'
025900031008     C                             + %trim(%editc(psjobnbr:'Z'))
026000031008     C                             + ' Tipo file inesistente.'
026100031008     C                   exsr      wrttivlt
026200031008     C                   eval      wrkesito = 'B'
026300031008     C*
026400000721     C                   else
026500000706     C* Reperisco il tipo file del cliente.
026600000706     C     k03vir02      setgt     tivir02l
026700000706     C     k02vir02      readpe    tivir02l
026800000706     C* Tipo file non previsto per il cliente
026900000706     C* oppure scaduto.
027000000731     C                   if        %eof
027100000706     C                             or virdtf < wrkoggi
027200000706     C                             or virdti > wrkoggi
027300000706     C*
027400000731     C                   exsr      inztivltp
027500000706     C*
027600000706     C                   eval      vltsts = '2'
027700000706     C                   eval      vltmsg =
027800000706     C                               %trim(sysname)
027900000706     C                             + '/'
028000000706     C                             + %trim(psjobname)
028100000706     C                             + '/'
028200000706     C                             + %trim(psuser)
028300000706     C                             + '/'
028400000706     C                             + %trim(%editc(psjobnbr:'Z'))
028500000706     C                             + ' Tipo file non previsto per il cliente.'
028600000706     C                   exsr      wrttivlt
028700000706     C                   eval      wrkesito = 'C'
028800000706     C*
028900000706     C                   else
029000000706     C*
029100000706     C* Imposto i default.
029200000706     C                   if        virpgt <> *blanks
029300000706     C                   eval      wrkpgt = virpgt
029400000706     C                   eval      wrkppt = virppt
029500000706     C                   else
029600000706     C                   eval      wrkpgt = vtfpgt
029700000706     C                   eval      wrkppt = vtfppt
029800000706     C                   endif
029900000712     C*
030000000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
030100000711     C                   if        wrkvarie = '0'                               <===================
030200000711     C                   if        virpgi <> *blanks                            <===================
030300000711     C                   eval      wrkpgi = virpgi                              <===================
030400000711     C                   eval      wrkppi = virppi                              <===================
030500000711     C                   else                                                   <===================
030600000711     C                   eval      wrkpgi = vtfpgi                              <===================
030700000711     C                   eval      wrkppi = vtfppi                              <===================
030800000711     C                   endif                                                  <===================
030900000711     C                   endif                                                  <===================
031000000706     C* File da tradurre.
031100000706     C                   if        virfid <> *blanks
031200000706     C                   eval      wrkfid = virfid
031300000706     C                   else
031400000706     C                   eval      wrkfid = vtffid
031500000706     C                   endif
031600000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
031700000706     C* File storico.
031800000706     C                   if        virfis <> *blanks
031900000706     C                   eval      wrkfis = virfis
032000000706     C                   else
032100000706     C                   eval      wrkfis = vtffis
032200000706     C                   endif
032300000706     C                   extrct    wrkoggiiso:*m wrkmese
032400000706     C                   extrct    wrkoggiiso:*d wrkday
032500000706     C                   eval      wrkmbs =
032600000706     C                             mc(wrkmese)
032700000706     C                             + wrkdaychr
032800000706     C                             + %subst(wrkksu:2:7)
032900030519     C*
033000030519     C                   eval      vssisv = virfi1
033100030519     C* Valorizzo subito il progressivo applicazione.
033200030519     C                   exsr      calprog
033300000706     C* File tradotto.
033400000706     C                   if        virfit <> *blanks
033500000706     C                   eval      wrkfit = virfit
033600000706     C                   else
033700000706     C                   eval      wrkfit = vtffit
033800000706     C                   endif
033900030526     C* Tipo nome membro tradotto
034000030526     C                   eval      dvir01 = virpth
034100070207     C* Se forzatura in ingresso adotto quella, altrimenti applico considerazioni standard
034200070207     C                   if        prmout <> *blanks
034300070207     C                   eval      wrkmbt = prmout                              membro = forzatura
034400070207     C                   else
034500030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
034600030526     C                   if        §VIR01TM = 'P'
034700030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
034800030526 xxx C***                eval      wrkmbs = dwlprg
034900030519     C                   else
035000070504     C                   if        §VIR01TM = 'T'
035100070504     C                   eval      wrkmbt = dwlprg                              membro = progressivo
035200070504     C                   eval      %subst(wrkmbt:3:2) = wrktip                  membro = prog + tipf
035300070504     C                   else
035400030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
035500070504     C                   endif
035600070207     C                   endif
035700030519     C                   endif
035800000706     C* Controllo abilitazione al servizio.
035900000706     C                   exsr      chkvss
036000000706     C                   endif
036100000706     C                   endif
036200000706     C*
036300000706     C                   endsr
036400000706
036500000711
036600000711
036700000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
036800000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
036900000711     C*
037000000711     C     exepgm        begsr
037100000711     C* Lancio il pgm in variabile con relativi parametri
037200000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
037300000711    C                   if        wrkpgi <> *blanks
037400000712     C* Chiamo il pgm post traduzione.
037500000712     C                   call(e)   'TIS7T9C1'
037600000712     C                   parm      wrkfid        prmfid
037700000712     C                   parm      wrkmbd        prmmbd
037800000712     C                   parm      wrkfis        prmfis
037900000712     C                   parm      wrkmbs        prmmbs
038000031106     C                   parm      wrkfit        prmfit
038100031106     C                   parm      wrkmbt        prmmbt
038200000712     C                   parm                    wrkpgi
038300000712     C                   parm                    wrkppi
038400000712     C                   parm      *blanks       prmesito
038500000712     C* Se la call è finita in errore
038600000712     C* o il lancio del PGM ha avuto degli errori.
038700000712     C                   if        %error
038800000712     C                             or prmesito <> '0'
038900000712     C     k01vlt        setll     tivlt07l
039000000712     C                   if        %found(tivlt07l)
039100000712     C     k01vlt        reade     tivlt07l
039200000712     C                   dow       not %eof
039300000712     C                   if        vltsqz = *zeros and
039400000712     C                             (vltsts = '0' or vltsts = '1')
039500000712     C                   eval      vltsts = '2'
039600000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
039700000712     C                   eval      vltmsg =
039800000712     C                               %trim(sysname)
039900000712     C                             + '/'
040000000712     C                             + %trim(psjobname)
040100000712     C                             + '/'
040200000712     C                             + %trim(psuser)
040300000712     C                             + '/'
040400000712     C                             + %trim(%editc(psjobnbr:'Z'))
040500000712     C                             + %trim(wrkmsg)
040600000712     C                   update    tivlt700
040700000712     C                   endif
040800000712     C     k01vlt        reade     tivlt07l
040900000712     C                   enddo
041000000712     C                   endif
041100000712     C                   endif
041200000712     C*
041300000711     C                   endif
041400000711     C*
041500000711     C                   endsr
041600000711
041700000711
041800000626
041900000626      /TITLE Valorizzazione Progressivo Applicazione
042000000626     C     calprog       begsr
042100000626     C*
042200000706     C                   if        dwlprg = *all'0'
042300000627     C                   read(e)   tis7prgf
042400000627     C                   if        not %error
042500000627     C                   eval      dwlprg = f_tis7prgf
042600000627     C*
042700000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
042800000719     C                   if        prmsrv <> *blanks
042900000719     C                   eval      vssisv = prmsrv
043000000719     C                   endif
043100000626     C*
043200000719     C                   if        prmprog = *blanks
043300051011     C                   move(p)   dwl_prg       wrkprg
043400000626     C                   add       1             wrkprg
043500051011     C                   move(p)   wrkprg        dwl_prg
043600051011     C                   movel     vssisv        dwl_isv
043700051207     C*                  if        wrktip <> *blanks
043800051207     C*                  movel     wrktip        dwl_tip
043900051207     C*                  else
044000051207     C*                  movel     'XX'          dwl_tip
044100051207     C*                  endif
044200040123     C                   eval      f_tis7prgf = dwlprg
044300040123     C                   update    tis7prg0
044400000626     C                   else
044500040123     C                   unlock(e) tis7prgf
044600000626     C                   eval      dwlprg = prmprog
044700000626     C                   endif
044800000627     C*
044900000627     C                   endif
045000000706     C                   endif
045100000626     C*
045200000626     C                   endsr
045300991104
045400000706
045500000706
045600000706
045700991104      /TITLE Controllo abilitazione servizio.
045800991104     C     chkvss        begsr
045900991104     C*
046000000914     C                   eval      vssok = *off
046100000914     C*
046200991105     C                   eval      vssksu = wrkksu
046300000914     C     k02vss02      setll     tivss02l
046400000914     C     k02vss02      reade     tivss02l
046500000914     C                   dow       not %eof
046600000914     C                   if        vssdde <= wrkoggi and
046700000914     C                             vssdsc >= wrkoggi and
046800000914     C                             vssvat <> 'A'     and
046900000914     C                             vssvat <> 'S'
047000000914     C                   eval      vssok = *on
047100000914     C                   leave
047200000914     C                   endif
047300000914     C     k02vss02      reade     tivss02l
047400000914     C                   enddo
047500000914     C*
047600000914     C                   if        vssok = *off
047700991104     C*
047800000622     C                   exsr      inztivlt
047900000329     C                   eval      vltsts = '2'
048000991104     C                   eval      vltmsg =
048100991104     C                               %trim(sysname)
048200991104     C                             + '/'
048300991104     C                             + %trim(psjobname)
048400991104     C                             + '/'
048500991104     C                             + %trim(psuser)
048600991104     C                             + '/'
048700991104     C                             + %trim(%editc(psjobnbr:'Z'))
048800991104     C                             + ' Cliente non abilitato al servizio.'
048900000622     C                   exsr      wrttivlt
049000000622     C                   eval      wrkesito = 'D'
049100031008     C*
049200000622     C                   else
049300031008     C*
049400031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
049500031008     C* del progressivo rispetto a file/membro e cliente
049600031008     C                   movel     '0'           chkPRG            1
049700031008     C     k08vlt        setll     tivlt08l
049800031008     C                   if        %equal(tivlt08l)
049900031008     C     k08vlt        reade     tivlt08l
050000031008     C                   dow       not %eof(tivlt08l)
050100031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
050200090414     C* (considero solamente i record in stato "da elaborare")
050300090414     C                   if        w_vltksc <> wrkksc AND
050400090414     C                             w_vltsts  = '0'
050500031008     C                   movel     '1'           chkPRG
050600031008     C                   endif
050700031008     C     k08vlt        reade     tivlt08l
050800031008     C                   enddo
050900031008     C                   endif
051000031008     C*
051100000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
051200031008     C                   if        chkPRG = '0'
051300000706     C                   exsr      traduci
051400031008     C                   else
051500031008     C*
051600031008     C                   exsr      inztivlt
051700031008     C                   eval      vltsts = '2'
051800031008     C                   eval      vltmsg =
051900031008     C                               %trim(sysname)
052000031008     C                             + '/'
052100031008     C                             + %trim(psjobname)
052200031008     C                             + '/'
052300031008     C                             + %trim(psuser)
052400031008     C                             + '/'
052500031008     C                             + %trim(%editc(psjobnbr:'Z'))
052600031008     C                             + ' Progressivo/membro NON univoco!!!'
052700031008     C                   exsr      wrttivlt
052800031008     C                   eval      wrkesito = 'F'
052900031008     C                   exsr      sndeml
053000031008     C                   exsr      uscita
053100031008     C                   endif
053200991104     C*
053300991104     C                   endif
053400991104     C*
053500991104     C                   endsr
053600000622
053700000622
053800000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
053900000622     C     rtvtia        begsr
054000000622     C*
054100000622     C                   eval      tbecod = 'ISV'
054200000731     C                   eval      tbeke1 = virfi1
054300000622     C     keytbe        chain     tntbe01l
054400000622     C                   if        %found(tntbe01l)
054500020212     C                   movel     tbeuni        disv
054600000622     C                   endif
054700000622     C*
054800000622     C                   endsr
054900000706
055000000706
055100000706
055200000706      /TITLE Traduzione.
055300000706     C     traduci       begsr
055400031105     C*
055500031105     C* Reperisco informazioni membro da tradurre.
055600031105     C                   eval      mbrd04a = wrkfid
055700031105     C                   eval      mbrd05  = wrkmbd
055800031105     C                   exsr      rtvmbrd
055900031105     C* Controllo se il membro da tradurre ha dei record.
056000031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
056100031105     C                   if        (qusei=*blanks and
056200060616     C                             qusnbrcr > 0) or prmnfp > '01'
056300060616     C                             or %trim(wrkfid) = '*'
056400030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
056500030512     C                   call      'TIS7T9R2'
056600031008     C                   parm                    wrkfit
056700031008     C                   parm                    wrkmbt
056800030512     C                   parm      '0'           chkFTP            1
056900031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
057000031105     C                   if        chkFTP = '0'
057100000706     C*
057200000706     C                   exsr      inztivlt
057300000706     C* Chiamo il pgm traduttore.
057400060616     C                   if        %trim(wrkfid) = '*' AND
057500060616     C                             %trim(wrkfis) = '*'
057600060616     C                   call(e)   'TIS7T9C3'
057700060616     C                   parm      wrkmbd        prmmbd
057800060616     C                   parm      wrkfit        prmfit
057900060616     C                   parm      wrkmbt        prmmbt
058000060616     C                   parm      wrkpgt        prmpgt
058100060616     C                   parm      wrkppt        prmppt
058200060616     C                   parm      *blanks       prmesito
058300060616     C                   else
058400000706     C                   call(e)   'TIS7T9C'
058500000706     C                   parm      wrkfid        prmfid
058600000706     C                   parm      wrkmbd        prmmbd
058700000706     C                   parm      wrkfis        prmfis
058800000706     C                   parm      wrkmbs        prmmbs
058900000706     C                   parm      wrkfit        prmfit
059000000706     C                   parm      wrkmbt        prmmbt
059100000706     C                   parm      wrkpgt        prmpgt
059200000706     C                   parm      wrkppt        prmppt
059300000706     C                   parm      *blanks       prmesito
059400060616     C                   endif
059500000706     C* Se la call è finita in errore
059600000706     C* o la traduzione ha avuto degli errori.
059700060320     C                   if        %error
059800060320     C                             or (prmesito <> '0' and
059900060320     C                                 prmesito <> 'Y')
060000000706     C                   eval      vltsts = '2'
060100000706     C                   eval      vltmsg =
060200000706     C                               %trim(sysname)
060300000706     C                             + '/'
060400000706     C                             + %trim(psjobname)
060500000706     C                             + '/'
060600000706     C                             + %trim(psuser)
060700000706     C                             + '/'
060800000706     C                             + %trim(%editc(psjobnbr:'Z'))
060900000706     C                             + ' Errori nella traduzione.'
061000000706     C                   else
061100060320     C                   if        prmesito = 'Y'
061200060320     C                   eval      wrkesito = prmesito
061300060320     C                   else
061400000706     C                   eval      wrkesito = '0'
061500060320     C                   endif
061600000706     C                   endif
061700000706     C*
061800000706     C                   exsr      wrttivlt
061900031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
062000031105     C                   else
062100030512     C                   exsr      inztivlt
062200030512     C                   eval      vltsts = '2'
062300030512     C                   eval      vltmsg =
062400030512     C                               %trim(sysname)
062500030512     C                             + '/'
062600030512     C                             + %trim(psjobname)
062700030512     C                             + '/'
062800030512     C                             + %trim(psuser)
062900030512     C                             + '/'
063000030512     C                             + %trim(%editc(psjobnbr:'Z'))
063100030512     C                             + ' Cliente/TipoFile in uso da server FTP'
063200030512     C                   exsr      wrttivlt
063300030512     C                   eval      wrkesito = 'E'
063400030512     C                   endif
063500031105     C* NO record da tradurre
063600031105     C                   endif
063700000706     C*
063800000706     C                   endsr
063900000328
064000000706
064100000706
064200000328      /TITLE Inizializzo variabili "wrk".
064300000328     C     inzwrk        begsr
064400000328     C*
064500000328     C                   clear                   wrkfit
064600000328     C                   clear                   wrkmbt
064700000328     C                   clear                   wrkpgi
064800000328     C                   clear                   wrkppi
064900000328     C                   clear                   wrkfis
065000000328     C                   clear                   wrkmbs
065100000328     C*
065200000328     C                   endsr
065300991026
065400000706
065500000706
065600991026      /TITLE Inizializzo TIVLT00F.
065700991026     C     inztivlt      begsr
065800000804     C*
065900000804     C                   exsr      rtvtia
066000991026     C*
066100000328     C                   clear                   tivlt000
066200991026     C                   eval      vltksc = wrkksc
066300991026     C                   eval      vltksu = wrkksu
066400000721     C                   eval      vlttip = wrktip
066500991026     C                   eval      vltfld = wrkfit
066600991026     C                   eval      vltmbd = wrkmbt
066700991026     C                   eval      vltpgi = wrkpgi
066800991026     C                   eval      vltppi = wrkppi
066900020212     C                   if        §isvproc = 'F'
067000000804     C                   eval      vltsnd = 'D'
067100000804     C                   else
067200991026     C                   eval      vltsnd = vtfsnd
067300000804     C                   endif
067400991026     C                   eval      vltfls = wrkfis
067500991026     C                   eval      vltmbs = wrkmbs
067600991026     C                   eval      vltdat = wrkoggi
067700991115     C                   eval      vltora = wrkora
067800991026     C                   eval      vltsts = '0'
067900991027     C                   eval      vltmsg =
068000991104     C                               %trim(sysname)
068100991104     C                             + '/'
068200991104     C                             + %trim(psjobname)
068300991027     C                             + '/'
068400991027     C                             + %trim(psuser)
068500991027     C                             + '/'
068600991027     C                             + %trim(%editc(psjobnbr:'Z'))
068700000328     C                   z-add     *zeros        vltsqz
068800000731     C*
068900991026     C                   endsr
069000991026
069100000706
069200000731
069300000731      /TITLE Inizializzo parzialmente TIVLT00F.
069400000731     C     inztivltp     begsr
069500000731     C*
069600000731     C                   clear                   tivlt000
069700000731     C                   eval      vltksc = wrkksc
069800000731     C                   eval      vltksu = wrkksu
069900000731     C                   eval      vlttip = wrktip
070000000731     C                   eval      vltpgi = wrkpgi
070100000731     C                   eval      vltppi = wrkppi
070200000731     C                   eval      vltsnd = vtfsnd
070300000731     C                   eval      vltdat = wrkoggi
070400000731     C                   eval      vltora = wrkora
070500000731     C                   eval      vltsts = '0'
070600000731     C                   eval      vltmsg =
070700000731     C                               %trim(sysname)
070800000731     C                             + '/'
070900000731     C                             + %trim(psjobname)
071000000731     C                             + '/'
071100000731     C                             + %trim(psuser)
071200000731     C                             + '/'
071300000731     C                             + %trim(%editc(psjobnbr:'Z'))
071400000731     C                   z-add     *zeros        vltsqz
071500000731     C*
071600000731     C                   exsr      rtvtia
071700000731     C*
071800000731     C                   endsr
071900000731
072000000731
072100000706
072200991026      /TITLE Scrittura TIVLT00F.
072300000731     C     wrttivlt      begsr
072400991026     C*
072500000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
072600000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
072700000731     C                   eval      vltisv = virfi1
072800020212     C                   eval      vlttia = §isvproc
072900000622     C                   eval      vltprg = dwlprg
073000000713     C                   eval      vltfl2 = prmnfp
073100991026     C                   write     tivlt000
073200040817     C*
073300050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
073400040817     C* procedura scambio dati
073500040817     C* ...quindi eseguo prima d tutto l'override
073600050909  "  C*
073700050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
073800050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
073900050909     C*                  eval      LUNG = %len(%trim(COMMAND))
074000050909  "  C*                  call(e)   'QCMDEXC'
074100050909     C*                  parm                    COMMAND
074200050909     C*                  parm                    LUNG             15 5
074300040817     C*
074400050909  "  C*                  if        not %error
074500050909     C*                  if        not %open(tivltnew)
074600050909     C*                  open(e)   tivltnew
074700050909     C*                  endif
074800050909  "  C*                  if        not %error
074900050909     C*                  write     tivltn00
075000050909     C*                  endif
075100050909     C*                  if        %open(tivltnew)
075200050909  "  C*                  close(e)  tivltnew
075300050909     C*                  endif
075400050909 TESTC*                  endif
075500991026     C*
075600991026     C                   endsr
075700991026
075800000706
075900000706
076000991026      /TITLE Operazioni finali.
076100991026     C     uscita        begsr
076200991026     C*
076300000627     C                   seton                                        lr
076400991026     C*
076500991026     C                   endsr
076600991026
076700000706
076800000706
076900991026      /TITLE Controllo formale parametri ricevuti.
077000991026     C     chkprm        begsr
077100991026     C*
077200991104     C                   if        wrkksu = *blanks
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
078400031008     C                             + ' Parametro codice cliente non passato.'
078500031008     C                   exsr      wrttivlt
078600031008     C                   eval      wrkesito = 'A'
078700031008     C*
078800991026     C                   exsr      uscita
078900991026     C                   endif
079000991026     C*
079100991026     C                   if        wrktip = *blanks
079200031008     C*
079300031008     C                   exsr      inztivlt
079400031008     C                   eval      vltsts = '2'
079500031008     C                   eval      vltmsg =
079600031008     C                               %trim(sysname)
079700031008     C                             + '/'
079800031008     C                             + %trim(psjobname)
079900031008     C                             + '/'
080000031008     C                             + %trim(psuser)
080100031008     C                             + '/'
080200031008     C                             + %trim(%editc(psjobnbr:'Z'))
080300031008     C                             + ' Parametro tipo file non passato.'
080400031008     C                   exsr      wrttivlt
080500031008     C                   eval      wrkesito = 'B'
080600031008     C*
080700991026     C                   exsr      uscita
080800991026     C                   endif
080900991026     C*
081000991026     C                   clear                   wrkesito
081100991026     C*
081200991026     C                   endsr
081300991111
081400000706
081500000706
081600991111      /TITLE Reperisco informazioni membro.
081700991111     C     rtvmbrd       begsr
081800991111     C* Pulisco e imposto la DS degli errori.
081900000124     C                   clear                   qusec
082000991111     C                   eval      qusbprv = %size(qusec)
082100991111     C*
082200991111     C                   call      'QUSRMBRD'
082300991111     C     qusm0200      parm                    mbrd01
082400991111     C                   parm                    mbrd02
082500991111     C                   parm                    mbrd03
082600991111     C                   parm                    mbrd04
082700991111     C                   parm                    mbrd05
082800991111     C                   parm                    mbrd06
082900991111     C                   parm                    qusec
083000991111     C*
083100991111     C                   endsr
083200031008
083300031008
083400031008
083500031008
083600031008      /TITLE Compongo il testo e spedisco una e-m@ail
083700031008     C     sndeml        begsr
083800031008     C*
083900031008     C* Inizializzo variabili
084000031008     C                   movel     *blanks       wrkEml          253
084100031008     C                   movel     *blanks       wrkEmlMsg      5000
084200031008     C                   movel     *blanks       wrkEmlOgg        44
084300031008     C* Valorizzo i campi della e-m@ail
084400120305     C                   eval      wrkEml='cedalert@brt.it'
084500031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
084600031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
084700031008     C                             'SU AS/400'                         +':/N'+
084800031008     C                             ' '                                 +':/N'+
084900031008     C                             'RIFERIMENTI'                       +':/N'+
085000031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
085100031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
085200031008     C                             'Cliente:'+vltksc                   +':/N'+
085300031008     C                             'TipoFile:'+vlttip                  +':/N'+
085400031008     C                             ' '                                 +':/N'+
085500031008     C                             'MESSAGGIO'                         +':/N'+
085600031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
085700031008     C*
085800031008     C                   call(e)   'TIS701C'
085900031008     C                   parm                    wrkEml
086000031008     C                   parm                    wrkEmlOgg
086100031008     C                   parm                    wrkEmlMsg
086200031008     C*
086300031008     C                   endsr
086400990910
086500000706
086600000706
086700990908      /TITLE Operazioni iniziali.
086800990908     C     *inzsr        begsr
086900990908     C*
087000990908     C     *entry        plist
087100991026     C     wrkksc        parm                    prmksc
087200991026     C     wrkksu        parm      wrkksu        prmksu
087300991026     C     wrktip        parm                    prmtip
087400991026     C                   parm      wrkesito      prmesito
087500000623     C                   parm                    prmprog
087600000623     C                   parm                    prmsrv
087700000628     C                   parm                    prmope
087800000713     C                   parm                    prmnfp
087900070301     C                   parm                    wprmout          10
088000070301     C*
088100070301     C* Gestisco il ricevimento parametri
088200070301     C                   eval      prmout = *blanks
088300070301     C                   if        %parms = 9
088400070301     C                   eval      prmout = wprmout
088500070301     C                   endif
088600991104     C*
088700991105     C     k02vss02      klist
088800991105     C                   kfld                    vssksu
088900991104     C                   kfld                    vssisv
089000991026     C*
089100991026     C     k03vir02      klist
089200991026     C                   kfld                    virksc
089300991026     C                   kfld                    virtip
089400991026     C                   kfld                    virdti
089500991026     C*
089600991026     C     k02vir02      klist
089700991026     C                   kfld                    virksc
089800991026     C                   kfld                    virtip
089900000706     C*
090000000706     C     k04vir02      klist
090100000706     C                   kfld                    vreksc
090200000706     C                   kfld                    vretfl
090300000706     C                   kfld                    vredti
090400990920     C*
090500990920     C     k01vtf01      klist
090600990920     C                   kfld                    vtftip
090700000622     C*
090800000622     C     keytbe        klist
090900000622     C                   kfld                    tbecod
091000000622     C                   kfld                    tbeke1
091100000712     C*
091200000712     C     k01vlt        klist
091300000712     C                   kfld                    vltprg
091400000712     C                   kfld                    vltksc
091500031008     C*
091600031008     C     k08vlt        klist
091700031008     C                   kfld                    wrkfit
091800031008     C                   kfld                    wrkmbt
091900000706     C*
092000991104     C* Reperisco nome sistema.
092100991104     C                   eval      qusbprv = %size(qusec)
092200991104     C                   call      'QWCRNETA'
092300991104     C                   parm                    rneta01
092400991104     C                   parm                    rneta02
092500991104     C                   parm                    rneta03
092600991104     C                   parm                    rneta04
092700991104     C                   parm                    qusec
092800991104     C                   eval      nait = rneta01c
092900990908     C*
093000990908     C                   endsr
093100991026** ctdata mc
093200991026ABCDEFGHILMN
