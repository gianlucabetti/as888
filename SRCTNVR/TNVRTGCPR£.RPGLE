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
001901050211     d totarr          s             10  0
001902050211     d totpar          s             10  0
002000050118
002100000607      *--------------------------------------------------------------------
002200050114
002300050117      * reperimento dati file giacenze in arrivo
002400050114      *
002500050117     c     *loval        setll     figca01l
002600000607     c                   do        *hival
002700050117     c                   read      figca01l
002800050117     c                   if        %eof(figca01l)
002900000607     c                   leave
003000000607     c                   end
003100050202     c                   add       1             contagca
003200050121      * prima di scrivere il record verifico che non sia un record vecchio
003300050121      * non pulito verificando la fase maggiore di 10 per cui giacenza
003400050121      * già trasmessa e se non esiste in partenza passo al record successivo
003500050121     c                   if        cgcpfas > 10 and cgcpfrg = 0
003600050121     c     kgcp2         chain     figcp02l
003700050121     c                   if        not %found(figcp02l)
003800050202     C                   seton                                        21
003900050202     C                   exsr      stampa
004000050202     c                   add       1             contaobs
004100050121     c                   iter
004200050121     c                   endif
004300050121     c                   endif
004400050118      *valorizza la ds del file per la scrittura del rekord
004500050126     c                   clear                   tigcp000
004600050118     c                   clear                   sede
004700050118     c                   movel     arri          sede
004800050118
004900050117     c                   exsr      oldfld
005000050118     c                   exsr      clrfld
005100050114     c                   exsr      newfld
005200050114      * scrivo il rekord del nuovo archivio
005300050117     c                   write     tigcp000
005400050202     c                   add       1             contanew
005500040507     c                   enddo
005600050117     c
005700050117      * reperimento dati file giacenze in partenza
005800050117      *
005900050117     c     *loval        setll     figcp01l
006000050117     c                   do        *hival
006100050117     c                   read      figcp01l
006200050117     c                   if        %eof(figcp01l)
006300050117     c                   leave
006400050117     c                   end
006500050202     c                   add       1             contagcp
006600050118      *valorizza la ds del file per la scrittura del rekord
006700050118     c                   clear                   TIGCP000
006800050118     c                   clear                   sede
006900050118     c                   movel     part          sede
007000050118
007100050118     c     ktigcp        chain     tigcp02l
007200050117     c
007300050118      *rekord già esistente sul nuovo file verifico come aggiornare con i
007400050118      * dati della partenza
007500050118     c                   if        %found(tigcp02l)
007600050118     c                   exsr      dati_part
007700050118     c                   update    tigcp000
007800050202     c                   add       1             contaagn
007900050118     c                   else
008000050118      * non trovato scrivo il rekord del nuovo archivio
008100050118     c                   exsr      oldfld
008200050118     c                   z-add     aGCPAAS       GCPAASA
008300050118     c                   z-add     aGCPLNP       GCPLNPA
008400050118     c                   z-add     aGCPNRS       GCPNRSA
008500050118     c                   z-add     aGCPNSP       GCPNSPA
008600050118     c                   z-add     aGCPLNA       GCPLNAA
008700050118     c                   write     tigcp000
008800050202     c                   add       1             contanew
008900050118     c                   endif
009000050117     c                   enddo
009100050118
009101050211     c                   eval      totarr = contagca - contaobs
009102050211     c                   eval      totpar = contagcp - contaagn
009200050202     c                   except    totali
009300000607     c                   seton                                        lr
009400040510     c
009500050117      *-------------------------------------------------
009600050117     c     oldfld        begsr
009700050117      *-------------------------------------------------
009800050117     c
009900050117     c                   move      aGCPATB       GCPATB
010000050117     c                   move      aGCPLNP       GCPLNP
010100050117     c                   move      aGCPAAS       GCPAAS
010200050117     c                   move      aGCPNRS       GCPNRS
010300050117     c                   move      aGCPNSP       GCPNSP
010400050117     c                   move      aGCPLNA       GCPLNA
010500050117     c                   move      aGCPAGC       GCPAGC
010600050117     c                   move      aGCPMGC       GCPMGC
010700050117     c                   move      aGCPFGC       GCPFGC
010800050117     c                   move      aGCPNGC       GCPNGC
010900050117     c                   move      aGCPFRG       GCPFRG
011000050117     c                   move      aGCPCMC       GCPCMC
011100050117     c                   move      aGCPCCR       GCPCCR
011200050117     c                   move      aGCPUBI       GCPUBI
011300050117     c                   move      aGCPSCM       GCPSCM
011400050117     c                   move      aGCPTCM       GCPTCM
011500050117     c                   move      aGCPFST       GCPFST
011600050117     c                   move      aGCPDMM       GCPDMM
011700050117     c                   move      aGCPDXD       GCPDXD
011800050117     c                   move      aGCPNRC       GCPNRC
011900050117     c                   move      aGCPDIS       GCPDIS
012000050117     c                   move      aGCPPSG       GCPPSG
012100050117     c                   move      aGCPASG       GCPASG
012200050117     c                   move      aGCPNPC       GCPNPC
012300050117     c                   move      aGCPVCS       GCPVCS
012400050117     c                   move      aGCPCAS       GCPCAS
012500050117     c                   move      aGCPVCA       GCPVCA
012600050117     c                   move      aGCPPPC       GCPPPC
012700050117     c                   move      aGCPAPC       GCPAPC
012800050117     c                   move      aGCPDDM       GCPDDM
012900050117     c                   move      aGCPDED       GCPDED
013000050117     c                   move      aGCPDLD       GCPDLD
013100050214     c                   move      aGCPDUR       GCPDUR
013101050210     C                   if        agcpdur = 0
013104050210     c                   clear                   gcpriap
013105050210     C                   ELSE
013107050210     c                   eval      gcpriap = 'R'
013108050210     C                   END
013200050117     c                   move      aGCPDCG       GCPDCG
013300050117     c                   move      aGCPCFG       GCPCFG
013400050117     c                   move      aGCPDLM       GCPDLM
013500050117     c                   move      aGCPTFG       GCPTFG
013600050117     c                   move      aGCPDIV       GCPDIV
013700050117     c                   move      aGCPSGS       GCPSGS
013800050117     c                   move      aGCPSGR       GCPSGR
013900050117     c                   move      aGCPSGD       GCPSGD
014000050117     c                   move      aGCPDSD       GCPDSD
014100050117     c                   move      aGCPDSA       GCPDSA
014200050117     c                   move      aGCPGGS       GCPGGS
014300050117     c                   move      aGCPSGF       GCPSGF
014400050117     c                   move      aGCPSGA       GCPSGA
014500050117     c                   move      aGCPTFT       GCPTFT
014600050117     c                   move      aGCPDBG       GCPDBG
014700050117     c                   move      aGCPLPW       GCPLPW
014800050117     c                   move      aGCPASW       GCPASW
014900050117     c                   move      aGCPNSW       GCPNSW
015000050117     c                   move      aGCPSGS       GCPSGS
015100050117     c                   move      aGCPNBW       GCPNBW
015200050117     c                   move      aGCPKSC       GCPKSC
015300050117     c                   move      aGCPDFT       GCPDFT
015400050117     c                   move      aGCPNFT       GCPNFT
015500050117     c                   move      aGCPFIV       GCPFIV
015600050117     c                   move      aGCPFAS       GCPFAS
015700050117     c                   endsr
015800050117      *-------------------------------------------------
015900050114     c     newfld        begsr
016000050114      *-------------------------------------------------
016100050117     c
016200050117      * questi campi sono relativi ai dati in arrivo e si riferiscono
016300050117      * a ex campi doppi oppure nuovi campi del nuovo tracciato
016400050117      * Qui bisogna gestire la scrittura condizionata dall'esistenza de
016500050117      * rekord in arrivo altrimenti va impostato il riferimento alla
016600050117      * bolla presente in sede ..
016700050118     c                   z-add     aGCPlpw       GCPLPWA
016800050118     c                   z-add     aGCPasw       GCPASWA
016900050118     c                   z-add     aGCPnsw       GCPNSWA
017000050118     c                   z-add     aGCPnbw       GCPNBWA
017100050118     c                   z-add     aGCPdlm       GCPDlma
017200050126     c                   z-add     aGCPdlm       GCPduma
017300050118     c                   z-add     aGCPDbg       GCPDEDa
017400050118     c                   z-add     aGCPAAS       GCPAASA
017500050118     c                   z-add     aGCPLNP       GCPLNPA
017600050118     c                   z-add     aGCPNRS       GCPNRSA
017700050118     c                   z-add     aGCPNSP       GCPNSPA
017800050118     c                   z-add     aGCPLNA       GCPLNAA
017900050118     c                   endsr
018000050118      *-------------------------------------------------
018100050118     c     clrfld        begsr
018200050118      *-------------------------------------------------
018300050118     c
018400050118     c                   clear                   GCPLPW
018500050118     c                   clear                   GCPASW
018600050118     c                   clear                   GCPNSW
018700050118     c                   clear                   GCPNBW
018800050118     c                   clear                   GCPDlM
018900050118     c                   clear                   GCPDbg
019000050118     c                   endsr
019100050118      *-------------------------------------------------
019200050118     c     dati_part     begsr
019300050118      *-------------------------------------------------
019301050307     c* imposto i campi che comunque devono essere presi dalla partenza
019302050307     c                   move      aGCPLNP       GCPLNP
019303050307     c                   move      aGCPAAS       GCPAAS
019304050307     c                   move      aGCPNRS       GCPNRS
019305050307     c                   move      aGCPNSP       GCPNSP
019306050307     c                   move      aGCPLNA       GCPLNA
019307050307     c                   move      aGCPTCM       GCPTCM
019308050307     c                   move      aGCPDMM       GCPDMM
019500050307      * se trovo discordanza di date per riapertura fra partenza e arrivo
019600050121      * verifico se la fase in partenza è maggiore uguale a 40 in questo
019700050121      * caso il rekord generato dall'arrivo è anomalo quindi lo ricopro con
019800050121      * i dati della partenza per chiudere la giacenza definitivamente
019801050307     c                   if        agcpdur <> gcpdur
019900050121     c                   if        agcpfas >= 40
020000050121     c                   exsr      oldfld
020100050121     c                   else
020200050201      *  ANOMALIA
020300050202     c                   add       1             contanom
020400050201     c                   seton                                        20
020500050201     c                   exsr      stampa
020600050121     c                   endif
020700050118     c
020800050118     c                   else
020900050118      * in caso di rek trovato verifico la fase e se è maggiore in partenza
021000050119      * aggiorno l'intero record altrimenti aggiorno solo i campi della
021100050119      * partenza
021200050118     c                   if        agcpfas > gcpfas
021300050118     c                   exsr      oldfld
021400050118     c                   else
021500050118     c                   move      aGCPLPW       GCPLPW
021600050118     c                   move      aGCPASW       GCPASW
021700050118     c                   move      aGCPNSW       GCPNSW
021800050118     c                   move      aGCPNBW       GCPNBW
021900050118     c                   move      aGCPDlM       GCPDlM
022000050118     c                   move      aGCPDbg       GCPDbg
022100050118     c                   endif
022200050118     c                   endif
022300050118     c                   endsr
022400050114      *-------------------------------------------------
022500050114     c     *inzsr        begsr
022600050114      *-------------------------------------------------
022700050117
022800050118     c     ktigcp        klist
022900050118     c                   kfld                    bgcpAGC
023000050118     c                   kfld                    bgcpFGC
023100050118     c                   kfld                    bgcpNGC
023200050118     c                   kfld                    bgcpFRG
023300050118     c     ktigcps       klist
023400050118     c                   kfld                    agcpAGC
023500050118     c                   kfld                    agcpFGC
023600050118     c                   kfld                    agcpNGC
023700050118     c                   kfld                    agcpFRG
023800050121     c     kgcp2         klist
023900050121     c                   kfld                    cgcpAGC
024000050121     c                   kfld                    cgcpFGC
024100050121     c                   kfld                    cgcpNGC
024200050121     c                   kfld                    cgcpFRG
024301050211     c                   except    testa
024400050114     c                   endsr
024500050201      *-------------------------------------------------
024600050201     c     stampa        begsr
024700050201      *-------------------------------------------------
024800050201     c   of              except    testa
024900050201     c                   eval      *inof = '0'
025000050202     c   20              except    riga
025100050202     c   21              except    rigaobs
025200050211     c                   setoff                                       2021
025300050201     c                   endsr
025400050211     Oprtf198   E            TESTa            01
025500050201     o                                            9 'TNVRTGCPR'
025600050201     O                                           65 'ANOMALIE TRASCODIFICA'
025700050203     O                                           +1 'GIACENZE FILIALE'
025800050201     O                       PAGE1         Z    132
025900050201     O          E            RIGA       01
026000050203     O                                           +0 'Giacenza disallineata tra '
026100050203     O                                           +0 'partenza e arrivo'
026300050201     O                       gcpagc              +1
026400050201     O                       gcpfgc              +1
026500050203     O                       gcpngc        z     +1
026600050201     O                                           +1 'Spedizione'
026700050201     O                       gcpaas              +1
026800050201     O                       gcplnp              +1
026900050201     O                       gcpnrs        z     +1
027000050201     O                       gcpnsp        z     +1
027100050202     O          E            RIGAobs    01
027200050202     O                                           +0 'Giacenza ARRIVO obsoleta:'
027300050202     O                       cgcpagc             +1
027400050202     O                       cgcpfgc             +1
027500050203     O                       cgcpngc       z     +1
027600050203     O                                           +1 'non trovata in PARTENZA,'
027700050202     O                                           +1 'Fase > 10, Progressivo = 0'
027800050211     O          E            totali     03
027900050202     o                                           +0 'Giacenze ARRIVO'
028000050203     O                       contagca      z     +1
028100050211     o                                           +1 'di cui obsolete'
028101050211     o                                           +1 'non trascodificate'
028200050203     O                       contaobs      z     +1
028201050211     o                                           +1 'TOTALE ARRIVO ('
028202050211     O                       contagca      z     +0
028203050211     o                                           +0 '-'
028204050211     O                       contaobs      z     +0
028205050211     o                                           +0 ')'
028206050211     O                       totarr        z     +1
028300050202     O          E            totali     01
028400050203     o                                           +0 'Giacenze PARTENZA'
028500050203     O                       contagcp      z     +1
028600050211     o                                           +1 'di cui con anomalie'
028601050211     o                                           +1 'trascodificate da verific'
028602050211     o                                           +0 'are'
028701050203     O                       contanom      z     +1
028702050211     o                                           +1 'di cui aggiornate'
028703050211     O                       contaagn      z     +1
028704050211     o                                           +1 'TOTOTALE PARTENZA ('
028705050211     O                       contagcp      z     +0
028706050211     o                                           +0 '-'
028707050211     O                       contaagn      z     +0
028708050211     o                                           +0 ')'
028709050211     O                       totpar        z     +1
028800050202     O          E            totali     01
029000050203     o                                           +0 'Giacenze TRASCODIFICATE'
029101050211     o                                           +1 '('
029200050211     O                       totarr        z     +0
029300050211     o                                           +0 '+'
029400050211     O                       totpar        z     +0
029500050211     o                                           +0 ')'
029700050211     O                       contanew      z     +1
