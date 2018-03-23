000100070618      *---------------------------------------------------------------*
000200070618      * Saldi letture pacchi civetta VanDerLande - Visualizza         *
000300070618      *---------------------------------------------------------------*
000400070618     h decedit('0,') datedit(*dmy/)
000500070618      *---------------------------------------------------------------*
000600070618      * Data base                                                     *
000700070618      *---------------------------------------------------------------*
000800070618     fFIMS12D   cf   e             workstn
000900070618     f                                     sfile(MS12S02:Nrr)
001000080620     fFIMSC01L  if   e           k disk
001100070618     fAZORG01L  if   e           k disk
001200070618     fTNTBE01L  if   e           k disk
001300070618     fTNTBE02L  if   e           k disk
001400070618     f                                     rename(TNTBE000:TNTBE002)
001500100212      *
001600100212     fWFMSC00F  o    e             disk    usropn
001700070618     fFIMS12P   o    e             printer oflind(*in01)
001800070618     f                                     usropn
001900070618      *---------------------------------------------------------------*
002000070618      * Data structure                                                *
002100070618      *---------------------------------------------------------------*
002200070618      *---
002300070618      * Costanti per funzioni IBM
002400070618      *---
002500080620     d MaxKey          c                   2
002600070618     d Ascendente      c                   1
002700070618     d Discendente     c                   2
002800070618     d Carattere       c                   6
002900070618     d Put             c                   1
003000070618     d EndPut          c                   2
003100070618     d Get             c                   3
003200070618      *
003300070618      *---
003400070618      * Schiere
003500070618      *---
003600070618      * - civette
003700070618     d cMSC            s              3  0 dim(20) inz                          *n° pacco
003800070618     d cDES            s             30    dim(20) inz                          *descrizione
003900081111     d cDAT            s             17    dim(20) inz                          *segnacollo
004000070618     d cPES            s              6  3 dim(20) inz                          *peso
004100070618     d cVOL            s              7  6 dim(20) inz                          *volume
004200070618      * - entrata baia
004300070618     d NrPep           s              2  0 dim(24) inz                          *n° pacco
004400070618     d DesPep          s              5    dim(24) inz                          *descrizione
004500070618      *
004600070618      *---
004700070618      * Strutture dati definite esternamente
004800070618      *---
004900070618      * - architettura
005000070618     d KPJBA         e ds                                                       *architettura
005100070618      * - ds procedura
005200070618     d dsMS00        e ds                  extname(FIMS00ds)                    *ds di procedura
005300070618      * - ds pacchi civetta
005400070618     d dMSC          e ds                  inz
005500070618      * - ds spunte macchinone
005600070618     d dMSL          e ds                  inz
005700070618      *
005800070618      * ds reperimento dato utente
005900070618      * - Parametri per richiamo al pgm di controllo profilo utenti
006000070618     D Tibs34Ds      e ds                  Inz
006100070618      * - Ds di riferimento al file esterno AzUte00F
006200070618     D AzuteDs       e ds                  ExtName(AzUte00F)
006300070618      * - Ds per dati organigramma
006400070618     D dDatiUte      e ds
006500070618      *
006600070618      *---
006700070618      * Strutture dati definite internamente
006800070618      *---
006900070618      * - ds composizione segnacollo civetta
007000070618     d                 ds                  inz
007100070618     d  dslnp                  1      3                                         *linea partenza
007200070618     d  dslna                  4      6                                         *linea arrivo
007300070618     d  dsnrs                  7      8                                         *serie
007400070618     d  dsnsc                  9     15                                         *numero
007500070618     d  dsznc                 16     17                                         *zona
007600081111     ***d  dsseg                  1     17                                         *dati segnacollo
007700081111     d  dsseg                  1     15                                         *dati segnacollo
007800070618      * - ds controlla data (8)
007900070618     d WLBda8          ds                  inz                                  *controlla data
008000070618     d  G08dat                        8  0 inz
008100070618     d  G08inv                        8  0 inz
008200070618     d  G08err                        1    inz('3')
008300070618     d  G08tgi                        5  0 inz
008400070618      * -
008500070618     d SflRcd          ds
008600070618     d  V2SDtl
008700070618     d  V2SOrl
008800070618     d  V2SDes
008900070618     d  V2SPep
009000070618     d  V2SVol
009100070618     d  V2SSvo
009200070618     d  V2SCvo
009300080620     d  Selected                      1a
009400070618      *
009500070618      *---
009600070618      * Variabili riferite al data base
009700070618      *---
009800070618     d KBEcod          s                   like(TBEcod)                         *tntbe01l
009900070618     d KBEke1          s                   like(TBEke1)
010000070618     d KBEke2          s                   like(TBEke2)
010100070618     d KBElin          s                   like(TBElin)
010200070618     d KBEsif          s                   like(TBEsif)
010300070618     d KSCfil          s                   like(MSCfil)                         *fnmsc00f
010400070618     d KSCdtl          s                   like(MSCdtl)
010500070618     d KSCorl          s                   like(MSCorl)
010600070618     d DEPdes          s                   like(§MSCdes)
010700070618     d DEPpes          s                   like(§MSCpes)
010800070618     d DEPvol          s                   like(§MSCvol)
010900070618      *---
011000070618      * Variabili
011100070618      *---
011200070618     d $Fine           s              1    inz(*off)                            *fine programma
011300070618     d $RecOK          s              1    inz(*off)                            *validità record
011400070618     d TipVid          s              1    inz('1')                             *video in gestione
011500070618     d xx              s              2  0 inz                                  *indice
011600070618     d xi              s              5  0 inz                                  *indice
011700070618     d wPos            s              2  0 inz
011800070618     d n14             s             14  0 inz                                  *numerico 14
011900070618     d n8              s              8  0 inz                                  *numerico 8
012000070618     d n3              s              3  0 inz                                  *numerico 3
012100070618     d DatCor          s              8  0 inz                                  *data correrte a/m/g
012200070618     d Nrr             s              4  0                                      *n° rel sf
012300070618     d StmNrr          s              4  0                                      *n° rel sf x stampa
012400070618     d wPerIni         s             12  0 inz                                  *data+ora INIZIALI
012500080620     d wPerInis        s             14  0 inz                                  *data+ora INIZIALI
012600070618     d wPerFin         s             12  0 inz                                  *data+ora FINALE
012700080620     d wPerFins        s             14  0 inz                                  *data+ora FINALE
012800080620     d wPerLet         s             14  0 inz                                  *data+ora LETTURA
012900070618     d §SClett         s             15  6 inz                                  *prg colli attuale
013000070618     d §SCreal         s             15  6 inz                                  *prg colli preced
013100070618     d §SCscos         s              3  0 inz                                  *% scost att/preced
013200070618     d §SCgap          s             18  6 inz                                  *% scost att/preced
013300100212     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
013400070618      *---
013500070618      * Variabili per funzioni IBM
013600070618      *---
013700070618     d NotUsed         s             16a
013800070618     d ReturnSize      s              9b 0
013900070618     d SizeList        s              9b 0
014000080620     d******* Selected        s              1a
014100020107
014200020107      /COPY QSYSINC/QRPGLESRC,QLGSORT
014300070618     d QLGKL                         16    dim(MaxKey)
014400070618     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
014500070618     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
014600070618     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
014700070618     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
014800020107
014900020107      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
015000020107      /COPY QSYSINC/QRPGLESRC,QUSEC
015100100212
015200100212       // -?Parametri API QCAPCMD (Process Commands)?
015300100212      /copy qsysinc/qrpglesrc,QCAPCMD
015400100212     d Qcmd            s            512    inz  varying
015500100212
015600100212       //--------------------------------------------------------------
015700100212       //?Definizione procedure usate.                                 ?
015800100212       //--------------------------------------------------------------
015900100212
016000100212       // -?API QCAPCMD (Process Commands)?
016100100212      /copy gaitrasrc/srcProtoPR,QCAPCMD
016200100212
016300070618      *
016400070618      * chiavi di lettura
016500070618     c     KeyTBE        klist                                                  *tntbe01l
016600070618     c                   kfld                    KBEcod                          -tabella
016700070618     c                   kfld                    KBEke1                          -chiave uno
016800070618     c                   kfld                    KBEke2                          -chiave due
016900070618     c                   kfld                    KBElin                          -lingua
017000070618     c                   kfld                    KBEsif                          -s.informativo
017100070618     c     Ke3TBE        klist                                                  *tntbe02l
017200070618     c                   kfld                    KBEcod                          -tabella
017300070618     c                   kfld                    KBElin                          -lingua
017400070618     c                   kfld                    KBEsif                          -s.informativo
017500070618     c                   kfld                    KBEke1                          -chiave uno
017600070618     c     KeyMSC        klist                                                  *fnmsa02l
017700070618     c                   kfld                    KSCfil                          -punto operativo
017800070618     c                   kfld                    KSCdtl                          -data lettura
017900070618     c                   kfld                    KSCorl                          -ora  lettura
018000070618     c     KeyTBE1       klist                                                  *tntbe01l
018100070618     c                   kfld                    KBEcod                          -tabella
018200070618     c                   kfld                    KBEke1                          -chiave uno
018300070618      *
018400070618      *---------------------------------------------------------------*
018500070618      * Main lines                                                    *
018600070618      *---------------------------------------------------------------*
018700070618      *
018800070618     c     *Entry        plist
018900070618     c                   parm                    KPJBA
019000070618     c                   parm                    dsMS00
019100070618      *
019200070618      * operazioni iniziali
019300070618     c                   exsr      OpeIni
019400070618      *
019500070618      * caricamento sfile
019600070618     c                   exsr      CarSfl
019700070618      *
019800070618      * gestione video
019900070618do  1c                   DOU       $Fine  = *on
020000070618cs  2c     TipVid        caseq     '1'           GesVd1                          *Sfile vuoto
020100070618     c     TipVid        caseq     '2'           GesVd2                          *Sfile
020200070618e   1c                   endcs
020300070618e   1c                   ENDDO
020400070618      *
020500070618     c                   eval      *inLR  = *on
020600070618      *---------------------------------------------------------------*
020700070618      * carsfl - caricamento sfile                                    *
020800070618      *---------------------------------------------------------------*
020900070618     c     CarSfl        BEGSR
021000070618      *
021100070618      * pulizia sfile
021200070618     c                   seton                                        3031      *non visualizza sfl
021300070618     c                   write     MS12C02
021400080807     c                   setoff                                         3181
021500070618      *
021600070618      * reimposta variabili di lavoro
021700080807     c                   clear                   $Nr2                           *SflRcdNbr
021800070618     c                   clear                   Nrr                            *n° record sfile
021900070618     c                   eval      TipVid = '1'                                 *gestione 1° video
022000070618      *
022100070618      * caricamento sfile -tutto-
022200070618     c                   z-add     D00fil        KSCfil                         *filiale
022300080620     c**!!!              z-add     D00dli        KSCdtl                         *data lettura
022400080620     c**!!!              z-add     D00oli        KSCorl                         *ora lettura
022500080620     c                   movel     wPerInis      kscdtl
022600080620     c                   move      wPerInis      kscorl
022700080620     c     KeyMSC        setll     FIMSC000
022800080620     c     KSCfil        reade     FIMSC000
022900080620do  1c                   DOW       NOT %eof(FIMSC01L)
023000070618     c                   exsr      chkrec                                       *controlla record
023100070618if  2c                   if        $RecOK = *on                                 *record valido
023200070618     c                   exsr      ScrSfl                                       *scrive sfile
023300070618e   2c                   endif
023400080620     c     KSCfil        reade     FIMSC000
023500070618e   1c                   ENDDO                                                  *fine file
023600080620     c                   eval      *in81  = %eof(FIMSC01L)
023700070618     c                   z-add     Nrr           StmNrr                         *n° rel sf x stampa
023800070618      *
023900070618      * imposta emissione sfile
024000070618if  1c                   if        Nrr > 0                                      *record scritti
024100070618     c                   movel     '2'           tipvid                         *gestione 2° video
024200070618     c                   eval      *in30  = *off                                *visualizza sfile
024300080807     c                   eval      $Nr2   = 1                                   *posiz. all'inizio
024400070618e   1c                   endif
024500070618      *
024600070618     c                   ENDSR
024700070618      *---------------------------------------------------------------*
024800070618      * chkrec - controlla record                                     *
024900070618      *---------------------------------------------------------------*
025000070618     c     ChkRec        BEGSR
025100070618      *
025200070618      * reimposta le variabili di lavoro
025300070618     c                   eval      $RecOK = *off                                *record valido
025400070618      *
025500070618      * periodo non richiesto, esclude
025600070618     c                   movel     MSCdtl        wPerLet                        *periodo FINALE
025700070618     c                   move      MSCorl        wPerLet                        *data + ora
025800080620if  1c                   IF        wPerLet < wPerInis OR
025900080620     c                             wPerLet > wPerFins
026000070618     c                   leavesr
026100070618e   1c                   ENDIF
026200070618      *
026300070618      * pacco civetta non richiesto, esclude
026400081111     ***c                   eval      xi     = 1
026500081111     ***c                   eval      *in28  = *off
026600081111     ***c     MSCseg        lookup    cDat(xi)                               28
026700081111if  1***c                   if        NOT *in28                                    *non trovato
026800081111     ***c                   leavesr
026900081111e   1***c                   endif                                                  *trovato
027000081111      /free
027100081111         xi = %lookup( %subst(MSCseg : 1 : 15) : cDAT );
027200081111         if  xi = *zero;
027300081111           leavesr;
027400081111         endif;
027500081111      /end-free
027600070618      *
027700070618     c                   movel     cDes(xi)      DEPdes
027800070618     c                   z-add     cPes(xi)      DEPpes
027900070618     c                   z-add     cVol(xi)      DEPvol
028000070618      *
028100070618     c                   eval      $RecOK = *on
028200070618      *
028300070618     c                   ENDSR
028400070618      *---------------------------------------------------------------*
028500070618      * scrsfl - scrive il sfile -tutto-                              *
028600070618      *---------------------------------------------------------------*
028700070618     c     ScrSfl        BEGSR
028800070618      *
028900070618     c                   clear                   MS12S02
029000070618      *
029100070618      * dati civetta LETTURA
029200070618     c                   reset                   WLBda8
029300070618     c                   eval      G08inv = MSCdtl
029400070618     c                   call      'XSRDA8'
029500070618     c                   parm                    WLBda8
029600070618     c                   eval      V2Sdtl = G08dat                              *data lettura
029700070618     c                   z-add     MSCorl        V2Sorl                         *ora  lettura
029800070618     c                   movel     DEPdes        V2Sdes                         *descrizione
029900070618     c                   z-add     MSCvol        V2Svol                         *volume lettura
030000070618      *
030100070618      * dati civetta STANDARDs
030200070618     c                   z-add     DEPvol        V2Scvo                         *volume civetta
030300070618      *
030400070618      * scostamento VOLUME: se la % di differenza è contenuta in +/-4%
030500070618      * non viene evidenziata alcuna % di scostamento
030600070618     c                   z-add     MSCvol        §SClett                        *volume lettura
030700070618     c                   z-add     DEPvol        §SCreal                        *volume reale
030800070618     c                   clear                   §SCscos                        *% scostamento
030900070618     c                   exsr      Scostam                                      *CALCOLA PERCENTUALE
031000070618if  1c                   if        %abs(§SCscos) > 4                            *se % <= 4
031100070618     c                   z-add     §Scscos       V2Ssvo                         *%scostamento VOLUME
031200070618e   1c                   endif
031300020625      * banco : recupero la tescrizione del numero baia caricato in schiera
031400070618     c                   z-add     1             xx
031500070618     c     MSCpep        lookup    NrPep(xx)                              35
031600070618     c   35              movel     DesPep(xx)    V2Spep
031700070618     c
031800070618      * scrive sfile
031900070618     c                   add       1             Nrr                            *incrementa n°rec
032000080807     c***                z-add     Nrr           $Nr2                           *posiziona in fondo
032100070618     c                   write     MS12S02                                      *scrive sfile
032200070618      *
032300070618     c                   ENDSR
032400070618      *---------------------------------------------------------------*
032500070618      * scostam - calcola la percentuale di scostamento               *
032600070618      *           INPUT  --> §sclett - > "X" lettura                  *
032700070618      *                      §screal - > "X" reali                    *
032800070618      *           OUTPUT --> §scscos - > % di scostamento             *
032900070618      *---------------------------------------------------------------*
033000070618     c     Scostam       BEGSR
033100070618      *---
033200070618      * (lettura - reale) = gap
033300070618      * (gap x 100) : lettura  = % scostamento
033400070618      *---
033500070618      *
033600070618      * NB: se lettura uguale a zero, lo imposta a uno per non dare
033700070618      *     errore
033800070618if  1c                   if        §SCreal  = *zeros
033900070618     c                   eval      §SCreal  = 1
034000070618e   1c                   endif
034100070618      *
034200070618      * calcola la differenza in termini percentuali
034300070618     c                   eval      §SCgap   = ((§SClett - §SCreal)
034400070618     c                                        * 100)
034500070618     c                                        / §SCreal
034600070618     c                   z-add     §SCgap        §SCscos                        *% scostamento
034700070618      *
034800070618      * Sistemazione della % scostamento se più di 3 cifre
034900070618sel 1c                   select
035000070618w   1c                   when      §SCgap  >= +1000                              *$ positiva
035100070618     c                   eval      §SCscos  = *hival
035200070618w   1c                   when      §SCgap  <= -1000                              *% negativa
035300070618     c                   eval      §SCscos  = *loval
035400070618e   1c                   endsl
035500070618      *
035600070618     c                   ENDSR
035700070618      *---------------------------------------------------------------*
035800070618      * gesvd1 - gestione prima videta -msg sfile vuoto-              *
035900070618      *---------------------------------------------------------------*
036000070618     c     GesVd1        BEGSR
036100070618      *
036200070618     c                   eval      *in30  = *on
036300070618     c                   write     MS12D00                                      *testata
036400070618     c                   write     MS12Z01                                      *funzioni
036500070618     c                   write     MS12V01                                      *video
036600070618     c                   exfmt     MS12C02                                      *sfile control
036700070618sel 1c                   select
036800070618      * F3 = Fine pgm
036900070618w   1c                   when      *inKC
037000070618     c                   exsr      F03ges
037100070618      * F12 = Ritorno
037200070618w   1c                   when      *inKL
037300070618     c                   exsr      F12ges
037400070618e   1c                   endsl
037500070618      *
037600070618     c                   ENDSR
037700070618      *---------------------------------------------------------------*
037800070618      * F03ges - Tasto funzionale F03 -> Fine programma               *
037900070618      *---------------------------------------------------------------*
038000070618     c     F03ges        BEGSR
038100070618      *
038200070618     c                   eval      $Fine  = *on                                 *fine pgm
038300070618     c                   eval      D00f03 = *on                                 *fine pgm
038400070618      *
038500070618     c                   ENDSR
038600070618      *---------------------------------------------------------------*
038700070618      * F12ges - Tasto funzionale F12 -> Ritorno                      *
038800070618      *---------------------------------------------------------------*
038900070618     c     F12ges        BEGSR
039000070618      *
039100070618     c                   eval      $Fine  = *on                                 *fine pgm
039200070618     c                   eval      D00f12 = *on                                 *fine pgm
039300070618      *
039400070618     c                   ENDSR
039500070618      *---------------------------------------------------------------*
039600070618      * gesvd2 - gestione seconda videta -sfile dati-                 *
039700070618      *---------------------------------------------------------------*
039800070618     c     GesVd2        BEGSR
039900080807      *
040000080807      * Posizionamento del cursore nel subfile
040100080807     c                   if        C02csr > *zero
040200080807     c                   eval      $Nr2   = C02csr
040300080807     c                   endif
040400100215      *
040500100215      * Abilitazione tasto funzionale F20
040600100215     c                   eval      *in20 = (%subst(KNMUS:1:5)='ISP99')
040700070618      *
040800070618      * emette il sfile
040900070618     c                   write     MS12D00                                      *testata
041000070618     c                   write     MS12Z02                                      *tasti funzionali
041100070618     c                   exfmt     MS12C02                                      *sfile control
041200070618sel 1c                   select
041300070618      * F3 = Fine pgm
041400070618w   1c                   when      *inKC
041500070618     c                   exsr      F03ges
041600070618     c                   leavesr
041700070618      * F5 = Ricarica
041800070618w   1c                   when      *inKE
041900070618     c                   exsr      F05ges
042000070618      * F11 = Cambia ordinamento
042100070618w   1c                   when      *inKK
042200070618     c                   exsr      F11ges
042300070618      * F12 = Ritorno
042400070618w   1c                   when      *inKL
042500070618     c                   exsr      F12ges
042600070618     c                   leavesr
042700100212      * F20 = Crea work-file WFMSC00F
042800100212w   1c                   when      *inKU
042900100212     c                   exsr      F20ges
043000070618      * F21 = Stampa dati
043100070618w   1c                   when      *inKV
043200070618     c                   exsr      F21ges
043300070618e   1c                   endsl
043400070618      *
043500070618     c                   ENDSR
043600070618      *---------------------------------------------------------------*
043700070618      * F05ges - Tasto funzionale F05 -> Ricarica                     *
043800070618      *---------------------------------------------------------------*
043900070618     c     F05ges        BEGSR
044000070618      *
044100070618      * ricarica il sfile
044200070618     c                   exsr      CarSfl
044300070618      *
044400070618     c                   ENDSR
044500070618      *---------------------------------------------------------------*
044600070618      * F11ges - Tasto funzionale F11 -> Cambio ordinamento (baia)    *
044700070618      *---------------------------------------------------------------*
044800070618     c     F11ges        BEGSR
044900070618      *
045000070618     c                   clear                   QLGSCB
045100070618     c                   clear                   QLGSCB00
045200070618      *
045300070618      * 1 campo chiave.
045400070618     c                   eval      QLGNBRK  = 1
045500070618      *
045600070618      * La baia è
045700080620      * a posizone 25   5 byte, char, ascending.
045800080620     c                   eval      QLGSP    = 25
045900070618     c                   eval      QLGSS    = %size(V2SPep)
046000070618     c                   eval      QLGDT    = Carattere
046100070618     c                   eval      QLGSO    = Ascendente
046200070618     c                   eval      QLGKL(1) = QLGSKL
046300070618      *
046400070618      * Load other sort parameters.
046500070618     c                   eval      QLGLB    = 80 + 16 * MaxKey
046600070618     c                   eval      QLGRL    = %size(SflRcd) - 1
046700070618     c                   eval      QLGRT    = 8
046800070618     c                   eval      QLGOKL   = 80
046900070618     c                   eval      QLGLKE   = 16
047000070618     c                   eval      QLGLSS   = 290
047100070618      *
047200070618      * Initialize Sort I/O API fields.
047300070618     c                   eval      QLGRL00  = QLGRL
047400070618     c                   eval      QLGRC00  = 1
047500070618     c                   clear                   QUSEI
047600070618     c                   eval      QUSBPRV  = %size(QUSEC)
047700070618      *
047800070618      * First step - Initialize the sort routine.
047900070618     c                   call      'QLGSORT'
048000070618     c                   parm                    QLGSCB
048100070618     c                   parm                    NotUsed
048200070618     c                   parm                    NotUsed
048300070618     c                   parm                    SizeList
048400070618     c                   parm                    ReturnSize
048500070618     c                   parm                    QUSEC
048600070618      *
048700070618      * Next step - Write records to I/O routine.
048800070618     c                   eval      QLGRT00  = Put
048900070618      *
049000070618do  1c                   DO        StmNrr        Nrr
049100070618      *
049200070618     c     Nrr           chain     Ms12S02
049300070618      *
049400070618      * Solo le righe con Selected = 'Y' sono riordinate,
049500070618      * quindi per fare un ordinamento di tutte le righe
049600070618      * metto 'Y' sempre.
049700070618     c                   eval      Selected = 'Y'
049800070618      *
049900070618     c                   clear                   QUSEI
050000070618     c                   eval      QUSBPRV  = %size(QUSEC)
050100070618     c                   call      'QLGSRTIO'
050200070618     c                   parm                    QLGSCB00
050300070618     c                   parm                    SflRcd
050400070618     c                   parm                    NotUsed
050500070618     c                   parm                    SizeList
050600070618     c                   parm                    NotUsed
050700070618     c                   parm                    QUSEC
050800070618      *
050900070618e   1c                   ENDDO
051000070618      *
051100070618      * Next step - Signal end of input, clear subfile for reload.
051200070618     c                   eval      QLGRT00  = EndPut
051300070618     c                   clear                   QUSEI
051400070618     c                   eval      QUSBPRV  = %size(QUSEC)
051500070618      *
051600070618     c                   call      'QLGSRTIO'
051700070618     c                   parm                    QLGSCB00
051800070618     c                   parm                    SflRcd
051900070618     c                   parm                    NotUsed
052000070618     c                   parm                    SizeList
052100070618     c                   parm                    NotUsed
052200070618     c                   parm                    QUSEC
052300070618      *
052400070618     c                   eval      *in30    = *on
052500070618     c                   eval      *in31    = *on
052600070618     c                   write     MS12C02
052700070618     c                   eval      *in31    = *off
052800070618      *
052900070618      * Final step - Write the records back to the subfile.
053000070618     c                   eval      QLGRT00  = Get
053100070618      *
053200070618do  1c                   DO        StmNrr        Nrr
053300070618     c                   clear                   QUSEI
053400070618     c                   eval      QUSBPRV  = %size(QUSEC)
053500070618     c                   call      'QLGSRTIO'
053600070618     c                   parm                    QLGSCB00
053700070618     c                   parm                    NotUsed
053800070618     c                   parm                    SflRcd
053900070618     c                   parm                    QLGRL00
054000070618     c                   parm                    NotUsed
054100070618     c                   parm                    QUSEC
054200070618      *
054300070618     c                   write     MS12S02
054400070618e   1c                   ENDDO
054500070618      *
054600070618     c                   eval      *in30    = *off
054700070618      *
054800070618     c                   ENDSR
054900070618      *---------------------------------------------------------------*
055000070618      * F21ges - Tasto funzionale F21 -> Stampa dati                  *
055100070618      *---------------------------------------------------------------*
055200070618     c     F21ges        BEGSR
055300070618      *
055400070618      * apre il file printer
055500070618     c                   open      FIMS12P
055600070618      *
055700070618      * imposta i campi dei parametri di lancio
055800070618     c                   z-add     V0Cfil        PACfil                         *punto operativo
055900070618     c                   movel     V0Dfil        PADfil                         *descrizione p.o
056000070618     c                   z-add     V0Cdli        PACdli                         *data INIZIALE
056100070618     c                   z-add     V0Coli        PAColi                         *ora  INIZIALE
056200070618     c                   z-add     V0Cdlf        PACdlf                         *data FINALE
056300070618     c                   z-add     V0Colf        PAColf                         *ora  FINALE
056400070618      *
056500070618      * stampa i parametri di lancio
056600070618     c                   write     MS12PT                                       *testata
056700070618     c                   write     MS12PA                                       *parametri
056800070618      *
056900070618      * stampa testata e intestazione
057000070618     c                   write     MS12PT                                       *testata
057100070618     c                   write     MS12PI                                       *intestazione
057200070618      *
057300070618      * legge i dati del sfile
057400070618     c                   clear                   Nrr
057500070618do  1c                   DO        STMnrr        Nrr
057600070618      *
057700070618     c     Nrr           chain     MS12S02
057800070618      *
057900070618      * imposta i campi di dettaglio
058000070618     c                   z-add     V2Sdtl        DECdtl
058100070618     c                   z-add     V2Sorl        DECorl
058200070618     c                   movel     V2Sdes        DECdes
058300070618     c                   move      V2Spep        DECpep
058400070618     c                   z-add     V2Svol        DECvol
058500070618     c                   z-add     V2Ssvo        DECsvo
058600070618     c                   z-add     V2Scvo        DECcvo
058700070618      *
058800070618      * fine pagina, stampa testata
058900070618if  2c                   if        *in01                                        *fine pagina
059000070618     c                   write     MS12RI                                       *riga chiusura
059100070618     c                   write     MS12PT                                       *testata
059200070618     c                   write     MS12PI                                       *intestazione
059300070618     c                   eval      *in01  = *off
059400070618e   2c                   endif
059500070618      *
059600070618      * stampa il dettaglio
059700070618     c                   write     MS12DE                                       *dettaglio
059800070618      *
059900000000e   1c                   ENDDO                                                  *fine sfile
060000070618      *
060100070618      * stampa il fine prospetto
060200070618     c                   write     MS12RI                                       *riga chiusura
060300070618     c                   write     MS12FI                                       *fine prospetto
060400070618      *
060500070618      * chiude il file printer
060600070618     c                   close     FIMS12P
060700070618      *
060800000000     c                   ENDSR
060900100212      /free
061000100212       //--------------------------------------------------------------
061100100212       // F20ges - Tasto funzionale F20 -> Creazione WorkFile con dati
061200100212       //--------------------------------------------------------------
061300100212       BEGSR  F20ges;
061400100212
061500100212         // -?Esecuzion pulizia del work-file WFMSC00F?
061600100212         Qcmd = 'clrpfm file(GAITRAAZM/WFMSC00F)';
061700100212         exsr  sr_ExecCmd;
061800100212         if  Qusei <> *blank;
061900100212           leavesr;
062000100212         endif;
062100100212
062200100212         // -?Apertura del workfile?
062300100212         open  WFMSC00F;
062400100212
062500100212         // -?Impostazione dei campi con i parametri di lancio?
062600100212         clear  WFMSC000;
062700100212         WSCute = KNMUS;
062800100212         WSCdat = DatCor;
062900100212         WSCfil = V0Cfil;
063000100212         WSCdli = D00dli;
063100100212         WSColi = V0Coli;
063200100212         WSCdlf = D00dlf;
063300100212         WSColf = V0Colf;
063400100212
063500100212         // -?Lettura dei dati del sfile?
063600100212         For  Nrr = 1  To  STMnrr;
063700100212
063800100212           chain  Nrr  MS12S02;
063900100212
064000100212           // -?Impostazione dei campi di dettaglio?
064100100212           wDate_ISO = %date(V2Sdtl : *eur);
064200100212           WSCdtl = %dec(wDate_ISO);
064300100212           WSCorl = V2Sorl;
064400100212           WSCdes = V2Sdes;
064500100212           evalr WSCpep = V2Spep;
064600100212           WSCvol = V2Svol;
064700100212           WSCsvo = V2Ssvo;
064800100212           WSCcvo = V2Scvo;
064900100212
065000100212           // -?Scrittura del record?
065100100212           //______________
065200100212           write  WFMSC000;
065300100212           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
065400100212
065500100212         EndFor;
065600100212
065700100212         // -?Chiusura del work-file?
065800100212         close  WFMSC00F;
065900100212
066000100212       ENDSR;
066100100212
066200100212       //--------------------------------------------------------------
066300100212       //?Esecuzione del comando (già impostato).                      ?
066400100212       //--------------------------------------------------------------
066500100212       BEGSR  sr_ExecCmd;
066600100212
066700100212         clear Qcap0100;
066800100212         Qcabcsdh = *off;
066900100212         Qcapa    = *off;
067000100212         Qcacmdss = *off;
067100100212         Qcaerved = *allX'00';
067200100212
067300100212         clear Qusec;
067400100212         Qusbprv  = %size(Qusec);
067500100212
067600100212         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
067700100212                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
067800100212                           0 : 0 : Qusec);
067900100212
068000100212         if  Qusei <> *blank;
068100100212           exsr  sr_PrintErr;
068200100212         endif;
068300100212
068400100212       ENDSR;
068500100212
068600100212       //--------------------------------------------------------------
068700100212       //?Stampa segnalazioni dell'errore rilevato.                    ?
068800100212       //--------------------------------------------------------------
068900100212       BEGSR  sr_PrintErr;
069000100212
069100100212         // -?Stampa Dump?
069200100212         Dump(A);
069300100212
069400100212         // -?Stampa Job-Log?
069500100212         Qcmd = 'DSPJOBLOG job(*) output(*print)';
069600100212         clear Qcap0100;
069700100212         Qcabcsdh = *off;
069800100212         Qcapa    = *off;
069900100212         Qcacmdss = *off;
070000100212         Qcaerved = *allX'00';
070100100212         clear Qusec;
070200100212         Qusbprv  = %size(Qusec);
070300100212         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
070400100212                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
070500100212                           0 : 0 : Qusec);
070600100212
070700100212       ENDSR;
070800100212
070900100212      /end-free
071000070618      *---------------------------------------------------------------*
071100070618      * OpeIni - operazioni iniziali                                  *
071200070618      *---------------------------------------------------------------*
071300070618     c     OpeIni        BEGSR
071400070618      *
071500020624      * Reperisco dati job
071600070618     c                   exsr      DatiJob
071700070618      * reperisce la data corrente
071800070618     c                   time                    n14                            *Ora (6)+ Data (8)
071900070618     c                   move      n14           n8                             *Data (8) in g/m/a
072000070618     c                   z-add     n8            G08dat
072100070618     c                   clear                   G08inv
072200070618     c                   movel     '0'           G08err
072300070618     c                   call      'XSRDA8'
072400070618     c                   parm                    WLBda8
072500070618     c                   z-add     G08inv        DatCor                         *Data corrente a/m/g
072600070618      *
072700070618      * caricamento tabelle occorrenti
072800070618     c                   exsr      CarTab
072900070618      *
073000070618      * imposta i periodi (data + ora) richiesti
073100070618     c                   movel     D00dli        wPerIni                        *periodo INIZIALE
073200070618     c                   move      D00oli        wPerIni                        *data + ora
073300080620     c                   movel     wPerIni       wPerInis
073400080620     c                   move      '00'          wPerInis
073500070618     c                   movel     D00dlf        wPerFin                        *periodo FINALE
073600070618     c                   move      D00olf        wPerFin                        *data + ora
073700080620     c                   movel     wPerFin       wPerFins
073800080620     c                   move      '59'          wPerFins
073900070618      *
074000070618      * imposta video testata
074100070618     c                   exsr      ImpVd0
074200070618      *
074300070618     c                   ENDSR
074400070618      *---------------------------------------------------------------*
074500070618      *  Reperisco Dati del job (Utente/Operativi)                    *
074600070618      *---------------------------------------------------------------*
074700070618     c     DatiJob       BEGSR
074800020624      *
074900070618     c     *dtaara       define    §azute        azuteds
075000070618     c     *dtaara       define    §datiute      ddatiute
075100020624      *
075200070618     c                   in(E)     *dtaara
075300070618     c                   IF        %Error or RSUT = *blanks
075400070618     c                   call      'TIBS34R'
075500070618     c                   parm                    Tibs34Ds
075600070618     c                   in        *dtaara
075700070618     c                   ENDIF
075800020624      *
075900070618     c                   ENDSR
076000070618      *---------------------------------------------------------------*
076100070618      * CarTab - caricamento tabelle occorrenti                       *
076200070618      *---------------------------------------------------------------*
076300070618     c     CarTab        BEGSR
076400070618      *---
076500070618      * Pacchi civetta
076600070618      *---
076700070618     c                   clear                   cMSC                           *azzera
076800070618     c                   clear                   cDES
076900070618     c                   clear                   cDAT
077000070618     c                   clear                   cPES
077100070618     c                   clear                   cVOL
077200070618     c                   clear                   xi
077300070618     c                   clear                   KBEcod                         *tabella
077400070618     c                   movel     *all'0'       KBEke1                         *chiave uno
077500070618     c                   move      'MSC'         KBEke1
077600070618     c                   clear                   KBEke2                         *chiave due
077700070618     c                   clear                   KBElin                         *lingua
077800070618     c                   clear                   KBEsif                         *s.i. di GRUPPO
077900070618     c     KeyTBE        chain     TNTBE000
078000070618if  1c                   if        NOT %found(TNTBE01L)                         *non trovato
078100070618     c                   movel(p)  KNSIF         KBEsif                         *s.i. di AZIENDA
078200070618     c     KeyTBE        chain     TNTBE000
078300070618e   1c                   endif
078400070618     c                   movel     'MSC'         KBEcod                         *tabella
078500070618     c                   clear                   KBElin                         *lingua
078600070618     c                   movel(p)  D00fil        KBEke1                         *chiave 1 = filiale
078700070618     c     Ke3TBE        setll     TNTBE002
078800070618     c     Ke3TBE        reade     TNTBE002
078900070618do  1c                   DOW       NOT %eof(TNTBE02L)
079000070618if  2c                   if        TBEatb = *blanks                             *no annullati
079100070618     c                   add       1             xi
079200070618     c                   movel     TBEke1        n3
079300070618     c                   z-add     n3            cMSC(xi)                       *n° pacco
079400070618     c                   movel     TBEuni        dMSC
079500070618     c                   movel     §MSCdes       cDES(xi)                       *descrizione
079600070618     c                   movel     §MSClnp       dsLNP
079700070618     c                   movel     §MSClna       dsLNA
079800070618     c                   movel     §MSCnrs       dsNRS
079900070618     c                   movel     §MSCnsc       dsNSC
080000070618     c                   movel     §MSCznc       dsZNC
080100070618     c                   movel     dsSEG         cDAT(xi)                       *segnacollo
080200070618     c                   z-add     §MSCpes       cPES(xi)                       *peso
080300070618     c                   z-add     §MSCvol       cVOL(xi)                       *volume
080400070618e   2c                   endif
080500070618     c     Ke3TBE        reade     TNTBE002
080600070618e   1c                   ENDDO
080700070618      *
080800070618      *---
080900070618      * Banco Entrata
081000070618      * Leggo tabella MSL per le applicazioni VDL/CML e carico su due
081100070618      * schiere numero entrata baia e relativa descr. del banco,
081200070618      * prendendo 5 caratteri successivi al carattere '_' .
081300070618      *---
081400070618     c                   clear                   NrPep                          *azzera
081500070618     c                   clear                   DesPep
081600070618     c                   clear                   xx
081700070618     c                   clear                   xi
081800070618     c                   clear                   KBEcod                         *tabella
081900070618     c                   movel     *all'0'       KBEke1                         *chiave uno
082000070618     c                   move      'MSL'         KBEke1
082100070618     c     KeyTBE1       chain     TNTBE000
082200070618if  1c                   if        NOT %found(TNTBE01L)                         *non trovato
082300070618     c                   movel(p)  KNSIF         KBEsif                         *s.i. di AZIENDA
082400070618     c     KeyTBE1       chain     TNTBE000
082500070618e   1c                   ENDIF
082600070618     c                   movel     'MSL'         KBEcod                         *tabella
082700070618     c                   clear                   KBElin                         *lingua
082800070618     c                   movel(p)  D00fil        KBEke1                         *chiave 1 = filiale
082900070618     c     Ke3TBE        setll     TNTBE002
083000070618     c     Ke3TBE        reade     TNTBE002
083100070618do  1c                   DOW       NOT %eof(TNTBE02L)
083200070618if  2c                   if        TBEatb = *blanks                             *no annullati
083300070618      * Carico numero e descrizione banco solo se applicazione
083400070618      * CML/VDL
083500070618     c                   clear                   dMSL
083600070618     c                   movel     tbeuni        dMSL
083700161026if  3c                   if        §MSLapl = 'V' or
083800161026     c                             §MSLap5 = 'V'
083900070618     c                   add       1             xx
084000070618     c                   z-add     §MSLent       NrPep(xx)
084100070618     c                   eval      wPos    = %scan('_':§MslPep)
084200070618if  4c                   if        wPos > 0 and wPos < 14
084300070618     c                   add       1             wPos
084400070618     c                   eval      DesPep(xx)=%subst(§MSLpep:wPos:5)
084500070618e   4c                   endif
084600070618e   3c                   endif
084700070618e   2c                   endif
084800070618     c     Ke3TBE        reade     TNTBE002
084900070618e   1c                   ENDDO
085000070618      *
085100070618     c                   ENDSR
085200070618      *---------------------------------------------------------------*
085300070618      * Impvd0 - imposta video testata                                *
085400070618      *---------------------------------------------------------------*
085500070618     c     ImpVd0        BEGSR
085600070618      *
085700070618     c                   z-add     D00fil        V0Cfil                         *punto operativo
085800070618     c     D00fil        chain     AZORG
085900070618if  1c                   if        %found(AZORG01L)
086000070618     c                   movel     ORGdes        V0Dfil                         *descrizione p.o
086100070618e   1c                   endif
086200070618      *
086300070618     c                   reset                   WLBda8
086400070618     c                   eval      G08inv = D00dli
086500070618     c                   call      'XSRDA8'
086600070618     c                   parm                    WLBda8
086700070618     c                   z-add     G08dat        V0Cdli                         *data INIZIALE
086800080620     c**!!!              z-add     D00oli        V0Coli                         *ora  INIZIALE
086900080620     c                   move      wPerInis      v0coli
087000070618      *
087100070618     c                   reset                   WLBda8
087200070618     c                   eval      G08inv = D00dlf
087300070618     c                   call      'XSRDA8'
087400070618     c                   parm                    WLBda8
087500070618     c                   z-add     G08dat        V0Cdlf                         *data FINALE
087600080620     c**!!!              z-add     D00olf        V0Colf                         *ora  FINALE
087700080620     c                   move      wPerFins      v0colf
087800070618      *
087900070618     c                   ENDSR
