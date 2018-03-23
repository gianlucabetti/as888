000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050118     ffigcp01l  if   e           k disk    rename(figcp000:figcp) prefix(b)
000400050121     ffigcp02l  if   e           k disk    rename(figcp000:figcp2) prefix(d)
000500050118     ffigca01l  if   e           k disk    rename(figcp000:figca) prefix(c)
000600050118     ftigcp02l  uf a e           k disk
000700050211     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000800050118
000900050118      *ds comodo per tracciati da trascodificare
001000050118     d sede          e ds                  extname(figcp00f) prefix(a)
001100050118     d part          e ds                  extname(figcp01l) prefix(b)
001200050118     d arri          e ds                  extname(figca01l) prefix(c)
001300050202     d*
001400050202     d contagca        s             10  0
001500050202     d contagcp        s             10  0
001600050202     d contanew        s             10  0
001700050202     d contaagn        s             10  0
001800050202     d contanom        s             10  0
001900050202     d contaobs        s             10  0
002000050211     d totarr          s             10  0
002100050211     d totpar          s             10  0
002200050118
002300000607      *--------------------------------------------------------------------
002400050114
002500050117      * reperimento dati file giacenze in arrivo
002600050114      *
002700050117     c     *loval        setll     figca01l
002800000607     c                   do        *hival
002900050117     c                   read      figca01l
003000050117     c                   if        %eof(figca01l)
003100000607     c                   leave
003200000607     c                   end
003300050202     c                   add       1             contagca
003400050121      * prima di scrivere il record verifico che non sia un record vecchio
003500050121      * non pulito verificando la fase maggiore di 10 per cui giacenza
003600050121      * già trasmessa e se non esiste in partenza passo al record successivo
003700050121     c                   if        cgcpfas > 10 and cgcpfrg = 0
003800050121     c     kgcp2         chain     figcp02l
003900050121     c                   if        not %found(figcp02l)
004000050202     C                   seton                                        21
004100050202     C                   exsr      stampa
004200050202     c                   add       1             contaobs
004300050121     c                   iter
004400050121     c                   endif
004500050121     c                   endif
004600050118      *valorizza la ds del file per la scrittura del rekord
004700050126     c                   clear                   tigcp000
004800050118     c                   clear                   sede
004900050118     c                   movel     arri          sede
005000050118
005100050117     c                   exsr      oldfld
005200050118     c                   exsr      clrfld
005300050114     c                   exsr      newfld
005400050114      * scrivo il rekord del nuovo archivio
005500050117     c                   write     tigcp000
005600050202     c                   add       1             contanew
005700040507     c                   enddo
005800050117     c
005900050117      * reperimento dati file giacenze in partenza
006000050117      *
006100050117     c     *loval        setll     figcp01l
006200050117     c                   do        *hival
006300050117     c                   read      figcp01l
006400050117     c                   if        %eof(figcp01l)
006500050117     c                   leave
006600050117     c                   end
006700050202     c                   add       1             contagcp
006800050118      *valorizza la ds del file per la scrittura del rekord
006900050118     c                   clear                   TIGCP000
007000050118     c                   clear                   sede
007100050118     c                   movel     part          sede
007200050118
007300050118     c     ktigcp        chain     tigcp02l
007400050117     c
007500050118      *rekord già esistente sul nuovo file verifico come aggiornare con i
007600050118      * dati della partenza
007700050118     c                   if        %found(tigcp02l)
007800050118     c                   exsr      dati_part
007900050118     c                   update    tigcp000
008000050202     c                   add       1             contaagn
008100050118     c                   else
008200050118      * non trovato scrivo il rekord del nuovo archivio
008300050118     c                   exsr      oldfld
008400050118     c                   z-add     aGCPAAS       GCPAASA
008500050118     c                   z-add     aGCPLNP       GCPLNPA
008600050118     c                   z-add     aGCPNRS       GCPNRSA
008700050118     c                   z-add     aGCPNSP       GCPNSPA
008800050118     c                   z-add     aGCPLNA       GCPLNAA
008900050118     c                   write     tigcp000
009000050202     c                   add       1             contanew
009100050118     c                   endif
009200050117     c                   enddo
009300050118
009400050211     c                   eval      totarr = contagca - contaobs
009500050211     c                   eval      totpar = contagcp - contaagn
009600050202     c                   except    totali
009700000607     c                   seton                                        lr
009800040510     c
009900050117      *-------------------------------------------------
010000050117     c     oldfld        begsr
010100050117      *-------------------------------------------------
010200050117     c
010300050117     c                   move      aGCPATB       GCPATB
010400050117     c                   move      aGCPLNP       GCPLNP
010500050117     c                   move      aGCPAAS       GCPAAS
010600050117     c                   move      aGCPNRS       GCPNRS
010700050117     c                   move      aGCPNSP       GCPNSP
010800050117     c                   move      aGCPLNA       GCPLNA
010900050117     c                   move      aGCPAGC       GCPAGC
011000050117     c                   move      aGCPMGC       GCPMGC
011100050117     c                   move      aGCPFGC       GCPFGC
011200050117     c                   move      aGCPNGC       GCPNGC
011300050117     c                   move      aGCPFRG       GCPFRG
011400050117     c                   move      aGCPCMC       GCPCMC
011500050117     c                   move      aGCPCCR       GCPCCR
011600050117     c                   move      aGCPUBI       GCPUBI
011700050117     c                   move      aGCPSCM       GCPSCM
011800050117     c                   move      aGCPTCM       GCPTCM
011900050117     c                   move      aGCPFST       GCPFST
012000050117     c                   move      aGCPDMM       GCPDMM
012100050117     c                   move      aGCPDXD       GCPDXD
012200050117     c                   move      aGCPNRC       GCPNRC
012300050117     c                   move      aGCPDIS       GCPDIS
012400050117     c                   move      aGCPPSG       GCPPSG
012500050117     c                   move      aGCPASG       GCPASG
012600050117     c                   move      aGCPNPC       GCPNPC
012700050117     c                   move      aGCPVCS       GCPVCS
012800050117     c                   move      aGCPCAS       GCPCAS
012900050117     c                   move      aGCPVCA       GCPVCA
013000050117     c                   move      aGCPPPC       GCPPPC
013100050117     c                   move      aGCPAPC       GCPAPC
013200050117     c                   move      aGCPDDM       GCPDDM
013300050117     c                   move      aGCPDED       GCPDED
013400050117     c                   move      aGCPDLD       GCPDLD
013500050214     c                   move      aGCPDUR       GCPDUR
013600050210     C                   if        agcpdur = 0
013700050314     c                   movel     agcpAGC       gcpdur
013800050314     c                   move      agcpMGC       gcpdur
013900050210     c                   clear                   gcpriap
014000050210     C                   ELSE
014100050210     c                   eval      gcpriap = 'R'
014200050210     C                   END
014300050117     c                   move      aGCPDCG       GCPDCG
014400050117     c                   move      aGCPCFG       GCPCFG
014500050117     c                   move      aGCPDLM       GCPDLM
014600050117     c                   move      aGCPTFG       GCPTFG
014700050117     c                   move      aGCPDIV       GCPDIV
014800050117     c                   move      aGCPSGS       GCPSGS
014900050117     c                   move      aGCPSGR       GCPSGR
015000050117     c                   move      aGCPSGD       GCPSGD
015100050117     c                   move      aGCPDSD       GCPDSD
015200050117     c                   move      aGCPDSA       GCPDSA
015300050117     c                   move      aGCPGGS       GCPGGS
015400050117     c                   move      aGCPSGF       GCPSGF
015500050117     c                   move      aGCPSGA       GCPSGA
015600050117     c                   move      aGCPTFT       GCPTFT
015700050117     c                   move      aGCPDBG       GCPDBG
015800050117     c                   move      aGCPLPW       GCPLPW
015900050117     c                   move      aGCPASW       GCPASW
016000050117     c                   move      aGCPNSW       GCPNSW
016100050117     c                   move      aGCPSGS       GCPSGS
016200050117     c                   move      aGCPNBW       GCPNBW
016300050117     c                   move      aGCPKSC       GCPKSC
016400050117     c                   move      aGCPDFT       GCPDFT
016500050117     c                   move      aGCPNFT       GCPNFT
016600050117     c                   move      aGCPFIV       GCPFIV
016700050117     c                   move      aGCPFAS       GCPFAS
016800050117     c                   endsr
016900050117      *-------------------------------------------------
017000050114     c     newfld        begsr
017100050114      *-------------------------------------------------
017200050117     c
017300050117      * questi campi sono relativi ai dati in arrivo e si riferiscono
017400050117      * a ex campi doppi oppure nuovi campi del nuovo tracciato
017500050117      * Qui bisogna gestire la scrittura condizionata dall'esistenza de
017600050117      * rekord in arrivo altrimenti va impostato il riferimento alla
017700050117      * bolla presente in sede ..
017800050118     c                   z-add     aGCPlpw       GCPLPWA
017900050118     c                   z-add     aGCPasw       GCPASWA
018000050118     c                   z-add     aGCPnsw       GCPNSWA
018100050118     c                   z-add     aGCPnbw       GCPNBWA
018200050118     c                   z-add     aGCPdlm       GCPDlma
018300050126     c                   z-add     aGCPdlm       GCPduma
018400050118     c                   z-add     aGCPDbg       GCPDEDa
018500050118     c                   z-add     aGCPAAS       GCPAASA
018600050118     c                   z-add     aGCPLNP       GCPLNPA
018700050118     c                   z-add     aGCPNRS       GCPNRSA
018800050118     c                   z-add     aGCPNSP       GCPNSPA
018900050118     c                   z-add     aGCPLNA       GCPLNAA
019000050118     c                   endsr
019100050118      *-------------------------------------------------
019200050118     c     clrfld        begsr
019300050118      *-------------------------------------------------
019400050118     c
019500050118     c                   clear                   GCPLPW
019600050118     c                   clear                   GCPASW
019700050118     c                   clear                   GCPNSW
019800050118     c                   clear                   GCPNBW
019900050118     c                   clear                   GCPDlM
020000050118     c                   clear                   GCPDbg
020100050118     c                   endsr
020200050118      *-------------------------------------------------
020300050118     c     dati_part     begsr
020400050118      *-------------------------------------------------
020500050307     c* imposto i campi che comunque devono essere presi dalla partenza
020600050307     c                   move      aGCPLNP       GCPLNP
020700050307     c                   move      aGCPAAS       GCPAAS
020800050307     c                   move      aGCPNRS       GCPNRS
020900050307     c                   move      aGCPNSP       GCPNSP
021000050307     c                   move      aGCPLNA       GCPLNA
021100050307     c                   move      aGCPTCM       GCPTCM
021101050407     c                   move      aGCPscm       GCPscm
021200050307     c                   move      aGCPDMM       GCPDMM
021400050121      * verifico se la fase in partenza è maggiore uguale a 40 in questo
021500050121      * caso il rekord generato dall'arrivo è anomalo quindi lo ricopro con
021600050121      * i dati della partenza per chiudere la giacenza definitivamente
021700050324     c                   if        agcpdur <> gcpdur and agcpdur <> 0
021800050121     c                   if        agcpfas >= 40
021900050121     c                   exsr      oldfld
022000050121     c                   else
022100050201      *  ANOMALIA
022200050202     c                   add       1             contanom
022300050201     c                   seton                                        20
022400050201     c                   exsr      stampa
022500050121     c                   endif
022600050118     c
022700050118     c                   else
022800050118      * in caso di rek trovato verifico la fase e se è maggiore in partenza
022900050119      * aggiorno l'intero record altrimenti aggiorno solo i campi della
023000050119      * partenza
023100050118     c                   if        agcpfas > gcpfas
023200050118     c                   exsr      oldfld
023300050118     c                   else
023400050118     c                   move      aGCPLPW       GCPLPW
023500050118     c                   move      aGCPASW       GCPASW
023600050118     c                   move      aGCPNSW       GCPNSW
023700050118     c                   move      aGCPNBW       GCPNBW
023800050118     c                   move      aGCPDlM       GCPDlM
023900050118     c                   move      aGCPDbg       GCPDbg
024000050118     c                   endif
024100050118     c                   endif
024101050407     c* le spese le prendo da chi le ha indipendentemente da tutto
024102050407      * se trovo discordanza di date per riapertura fra partenza e arrivo
024104050407     c                   if        agcpsgr > gcpsgr or
024105050407     c                             agcpsgs > gcpsgs or
024106050407     c                             agcpsgd > gcpsgd or
024107050407     c                             agcpsga > gcpsga
024108050407     c                   eval      gcpsgr = agcpsgr
024109050407     c                   eval      gcpsgs = agcpsgs
024110050407     c                   eval      gcpsgd = agcpsgd
024111050407     c                   eval      gcpsga = agcpsga
024112050407     c                   eval      gcpggs = agcpggs
024113050407     c                   eval      gcpsgf = agcpsgf
024114050407     c                   eval      gcptft = agcptft
024115050407     c                   eval      gcpdsd = agcpdsd
024116050407     c                   eval      gcpdsa = agcpdsa
024117050407     c                   else
024118050407     c                   eval      agcpsgr = gcpsgr
024119050407     c                   eval      agcpsgs = gcpsgs
024120050407     c                   eval      agcpsgd = gcpsgd
024121050407     c                   eval      agcpsga = gcpsga
024122050407     c                   eval      agcpggs = gcpggs
024123050407     c                   eval      agcpsgf = gcpsgf
024124050407     c                   eval      agcptft = gcptft
024125050407     c                   eval      agcpdsd = gcpdsd
024126050407     c                   eval      agcpdsa = gcpdsa
024127050407     c                   end
024200050118     c                   endsr
024300050114      *-------------------------------------------------
024400050114     c     *inzsr        begsr
024500050114      *-------------------------------------------------
024600050117
024700050118     c     ktigcp        klist
024800050118     c                   kfld                    bgcpAGC
024900050118     c                   kfld                    bgcpFGC
025000050118     c                   kfld                    bgcpNGC
025100050118     c                   kfld                    bgcpFRG
025200050118     c     ktigcps       klist
025300050118     c                   kfld                    agcpAGC
025400050118     c                   kfld                    agcpFGC
025500050118     c                   kfld                    agcpNGC
025600050118     c                   kfld                    agcpFRG
025700050121     c     kgcp2         klist
025800050121     c                   kfld                    cgcpAGC
025900050121     c                   kfld                    cgcpFGC
026000050121     c                   kfld                    cgcpNGC
026100050121     c                   kfld                    cgcpFRG
026200050211     c                   except    testa
026300050114     c                   endsr
026400050201      *-------------------------------------------------
026500050201     c     stampa        begsr
026600050201      *-------------------------------------------------
026700050201     c   of              except    testa
026800050201     c                   eval      *inof = '0'
026900050202     c   20              except    riga
027000050202     c   21              except    rigaobs
027100050211     c                   setoff                                       2021
027200050201     c                   endsr
027300050211     Oprtf198   E            TESTa            01
027400050201     o                                            9 'TNVRTGCPR'
027500050201     O                                           65 'ANOMALIE TRASCODIFICA'
027600050203     O                                           +1 'GIACENZE FILIALE'
027700050201     O                       PAGE1         Z    132
027800050201     O          E            RIGA       01
027900050203     O                                           +0 'Giacenza disallineata tra '
028000050203     O                                           +0 'partenza e arrivo'
028100050201     O                       gcpagc              +1
028200050201     O                       gcpfgc              +1
028300050203     O                       gcpngc        z     +1
028400050201     O                                           +1 'Spedizione'
028500050201     O                       gcpaas              +1
028600050201     O                       gcplnp              +1
028700050201     O                       gcpnrs        z     +1
028800050201     O                       gcpnsp        z     +1
028900050202     O          E            RIGAobs    01
029000050202     O                                           +0 'Giacenza ARRIVO obsoleta:'
029100050202     O                       cgcpagc             +1
029200050202     O                       cgcpfgc             +1
029300050203     O                       cgcpngc       z     +1
029400050203     O                                           +1 'non trovata in PARTENZA,'
029500050202     O                                           +1 'Fase > 10, Progressivo = 0'
029600050211     O          E            totali     03
029700050202     o                                           +0 'Giacenze ARRIVO'
029800050203     O                       contagca      z     +1
029900050211     o                                           +1 'di cui obsolete'
030000050211     o                                           +1 'non trascodificate'
030100050203     O                       contaobs      z     +1
030200050211     o                                           +1 'TOTALE ARRIVO ('
030300050211     O                       contagca      z     +0
030400050211     o                                           +0 '-'
030500050211     O                       contaobs      z     +0
030600050211     o                                           +0 ')'
030700050211     O                       totarr        z     +1
030800050202     O          E            totali     01
030900050203     o                                           +0 'Giacenze PARTENZA'
031000050203     O                       contagcp      z     +1
031100050211     o                                           +1 'di cui con anomalie'
031200050211     o                                           +1 'trascodificate da verific'
031300050211     o                                           +0 'are'
031400050203     O                       contanom      z     +1
031500050211     o                                           +1 'di cui aggiornate'
031600050211     O                       contaagn      z     +1
031700050211     o                                           +1 'TOTOTALE PARTENZA ('
031800050211     O                       contagcp      z     +0
031900050211     o                                           +0 '-'
032000050211     O                       contaagn      z     +0
032100050211     o                                           +0 ')'
032200050211     O                       totpar        z     +1
032300050202     O          E            totali     01
032400050203     o                                           +0 'Giacenze TRASCODIFICATE'
032500050211     o                                           +1 '('
032600050211     O                       totarr        z     +0
032700050211     o                                           +0 '+'
032800050211     O                       totpar        z     +0
032900050211     o                                           +0 ')'
033000050211     O                       contanew      z     +1
