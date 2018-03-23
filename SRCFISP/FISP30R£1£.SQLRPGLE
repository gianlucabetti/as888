000100101220       //==============================================================
000200101220       //?  STORE PROCEDURE - DATI PER IMMAGINI LDV (TERMINALINI AUT)  ?
000300130411       //?                    (ANCHE RIGENERAZIONE)                    ?
000400101220       //==============================================================
000500101220       //
000600101220       //? QUESTO PROGRAMMA È "LEGATO"? A "FNLSB6R" (STAMPA LDV A4).  ?
000700130214       //? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSO !!  ?
000800101220       //
000900130924       //ATTENZIONE:?I CAMPI NEL RECORD LSB6LDV RISULTANO IMPOSTATI
001000101220       //             NELLE VARIE STRUTTURE DATI FISP30nnDS - TUTTI
001100101220       //             ALFANUMERICI !
001200130924       //
001300130924       //NOTA BENE (a):?Valori previsti nel parm di output RISULTATO:?
001400130924       //               ?· 1) "OK" => Generare l'immagine             ?
001500130924       //               ?· 2) "NO" => Ripetere la richiesta           ?
001600130924       //               ?· 3) "XX" => NON ripetere la richiesta       ?
001700101220       //
001800130924       //NOTA BENE (b):?> Cod. cliente 0501331 => FedEx
001900150430       //                  cod. fissato nella costante "c_CliFedEx"
002000150430       //                > Codice cliente 0506514 => FedEx (2)
002100150430       //                  codice fissato nella costante "c_CliFedEx2"
002200130924       //                > Cod. cliente 0201515 => PraOil oleodotti
002300150430       //                  cod. fissato nella costante "c_CliPraOil"
002400130924       //                > Cod. cliente 0504450 => TTL
002500130924       //                  cod. fissato nella costante "c_CliTTL"
002600130924       //                > Cod. cliente 0632910 => Consorzio Lotterie Naz
002700130924       //                  cod. fissato nella costante "c_CliCLN"
002800140513       //                > Cod. cliente 0017093 => Lotterie Nazionali
002900140513       //                       (gemello di backup su Bologna)
003000140513       //                  cod. fissato nella costante "c_CliLN_2"
003100130924       //                > Cod. cliente 0634923 => S G Soluzioni Logist.
003200130924       //                  cod. fissato nella costante "c_CliSG "
003300130924       //                > Cod. cliente 0635042 => S G Soluzioni Logist.
003400130924       //                  cod. fissato nella costante "c_CliSG_2"
003500101220       //
003600101220       //==============================================================
003700101213
003800101213       //--------------------------------------------------------------
003900101213       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004000101213       //--------------------------------------------------------------
004100101213
004200130411     /*PRM  dbgview(*source)
004300130411     /*PRM  commit(*NONE)
004400130411     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
004500130411     /*CMD         ovrscope(*calllvl)
004600130411     /*END  dltovr file(TITAS30C) lvl(*)
004700101213     /*END
004800101213
004900101213       //--------------------------------------------------------------
005000101213       //?Specifiche di controllo.                                     ?
005100101213       //--------------------------------------------------------------
005200080430
005300080430     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005400080718     h dftactgrp(*no) actgrp(*caller)
005500080430
005600101213       //--------------------------------------------------------------
005700101213       //?Dichiarazione file.                                          ?
005800101213       //--------------------------------------------------------------
005900080430
006000101213       // -?Tabelle?
006100101213     fTABEL00F  if   e           k disk    usropn
006200101213
006300101213       // -?Organigramma di filiale?
006400101213     fAZORG01L  if   e           k disk    usropn
006500130411
006600130411       // -?Distinte delle cui LdV ri-generare le immagini?
006700130422     fFIIRD11L  if   e           k disk
006800130411
006900130411       // -?Dati per LdV (sede)?
007000130411     fTITAS30C  if   e           k disk    usropn
007100130411     f                                     extfile(wLibFileTITAS)
007200101213
007300101213       // -?Dati per LdV arrivo?
007400070305     fFNARB01L  if   e           k disk    usropn
007500070306     fFNART01L  if   e           k disk    usropn
007600131206     fFNEVB04L  if   e           k disk    usropn
007700140303     fFNEVB22L  if   e           k disk    usropn
007800080710     fFIDST01L  if   e           k disk    usropn
007900101213
008000101213       // -?Altri dati comuni?
008100090619     fFNLBL01L  if   e           k disk    usropn
008200081030     fFIARS01L  if   e           k disk    usropn
008300070305     fFIAR401L  if   e           k disk    usropn
008400030515     fFIAR501L  if   e           k disk    usropn
008500070305     fFIAR601L  if   e           k disk    usropn
008600070305     fFIAR701L  if   e           k disk    usropn
008700070305     fFIAR901L  if   e           k disk    usropn
008800110117
008900131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009000110117     fFNBLP01L  if   e           k disk    usropn
009100101213
009200101207       // -?O.R.M.?
009300100517     fFNORM01L  if   e           k disk    usropn
009400101213
009500110309       // -?File LOG spedizioni elaborate per immagine LdV?
009600110202     fFIILV01L  Uf A e           k disk    usropn
009700130227
009800130227       // -?Spool da inviare via e-mail:?
009900130227       //   ·?Corpo del messaggio di posta elettronica?
010000130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010100101213
010200101207       // -?Printer File  (se richiesta bozza in stampa)?
010300090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
010400030515
010500101213       //--------------------------------------------------------------
010600101213       //?Definizione costanti.                                        ?
010700101213       //--------------------------------------------------------------
010800030515
010900110117     d c_DateMax_A     c                   const('20391231')
011000131008       // -?Costanti in output?
011100110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
011200030515     d                                     NON SEQUENZIALI')
011300110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
011400030717     d                                     N. ')
011500110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
011600070315     d                                     TOTRASPORTATORE')
011700101213       // -?Codice cliente "FedEx"?
011800150430     d c_CliFedEx      c                   const(0501331)
011900150430     d c_CliFedEx2     c                   const(0506514)
012000101213       // -?Codice cliente "PraOil oleodotti italiani"?
012100150430     d c_CliPraOil     c                   const(0201515)
012200101213       // -?Codice cliente "TTL"?
012300091116     d c_CliTTL        c                   const(0504450)
012400101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
012500090619     d c_CliCLN        c                   const(0632910)
012600140513     d c_CliLN_2       c                   const(0017093)
012700101213       // -?Codice cliente "S G Soluzioni Logistiche"?
012800090619     d c_CliSG         c                   const(0634923)
012900091117     d c_CliSG_2       c                   const(0635042)
013000110923
013100110923       // -?Consegna CON Riserva specifica?
013200110923     d c_RiservaSpecif...
013300110923     d                 c                   const('RS')
013400091117
013500131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
013600091117     d c_NoContr       c                   const('NO CONTRATTO')
013700101213
013800101213       // -?Costante per controllo "caratteri solo numerici"?
013900110126     d c_Digits        c                   const('0123456789')
014000101207       // -?Numero di righe da gestire per la LdV?
014100130213     d c_Row           c                   const(36)
014200081027     d c_Row_B         c                   const(49)
014300130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
014400081030     d c_Row_Fed       c                   const(10)
014500101207       // -?Sequenza numerica?
014600080430     d c_Char          c                   const('*...+....1....+....2+
014700080430     d                                            ....+....3....+....4+
014800080430     d                                            ....+....5....+....6+
014900080430     d                                            ....+....7....+....8+
015000080430     d                                            ....+....9....+...10+
015100080430     d                                            ....+...11....+...12')
015200130227
015300130227       // -?Dati di default?
015400130227     d c_Mittente      c                   const('ced')
015500130227     d c_at            c                   const('@brt.it')
015600130227     d c_Sede          c                   const('046')
015700130227
015800130227       // -?Comandi di override al PrtF "corpo e-mail"?
015900130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016000130227     d                                           file(PRTEMAIL) +
016100130227     d                                           pagesize(66 132) +
016200130227     d                                           lpi(6) cpi(10) +
016300130227     d                                           ovrscope(*actgrpdfn) +
016400130227     d                                           ')
016500130227     d c_CmdDltOvr     c                   const('DLTOVR +
016600130227     d                                            file(PRTEMAIL) +
016700130227     d                                            lvl(*actgrpdfn)')
016800101213
016900101213       //--------------------------------------------------------------
017000101213       //?Definizione schiere.                                         ?
017100101213       //--------------------------------------------------------------
017200101213
017300101213       // -?Giorni della settimana e relativa decodifica?
017400070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
017500070306     d $Gio            s             15    dim(9)   alt($GgS)
017600110202
017700110202       // -?Codice / Decodifica LOG x FIILV00F?
017800110202     d $CoL            s                   like(ILVcol)
017900130313     d                                     dim(10)  ctdata  perrcd(1)
018000110202     d $DeL            s                   like(ILVnot)
018100130313     d                                     dim(10)  alt($CoL)
018200101213
018300101207       // -?Dati da restituire?
018400130214     d LdV             ds           123    occurs(c_Row_B2) inz
018500080508     d   LdvPrg                       3s 0 inz
018600080430     d   LdvTxt                     120    inz
018700101213
018800101213       // -?Note e Particolarità?
018900070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019000101213       // -?Consegne Precedenti?
019100070319     d $CP             s                   dim(20)  inz(*all'9')
019200070319     d                                                   like(Elem_CP)
019300070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
019400101213
019500101213       //--------------------------------------------------------------
019600101213       //?Definizione strutture dati.                                  ?
019700101213       //--------------------------------------------------------------
019800101213
019900080430     d KPJBA         e ds                  inz
020000101213
020100131008       // -?Dati LDV da restituire per l'immagine?
020200080506     d FNLSB5ds      e ds                  inz
020300080430
020400110202       // -?Records in output?
020500110202       // ·? 1ª riga?
020600081021     d ds_01         e ds                  inz  extname(FISP3001ds)
020700081021     d                                          prefix(P : 3)
020800110202       // ·? 2ª riga (vuota)?
020900110202       // ·? 3ª riga?
021000080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021100080430     d                                          prefix(P : 3)
021200110202       // ·? 4ª riga?
021300080430     d ds_04         e ds                  inz  extname(FISP3004ds)
021400080430     d                                          prefix(P : 3)
021500110202       // ·? 5ª riga?
021600080430     d ds_05         e ds                  inz  extname(FISP3005ds)
021700080430     d                                          prefix(P : 3)
021800110202       // ·? 6ª riga (vuota)?
021900110202       // ·? 7ª riga?
022000080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022100080430     d                                          prefix(P : 3)
022200110202       // ·? 8ª riga (vuota)?
022300110202       // ·? 9ª riga?
022400080430     d ds_09         e ds                  inz  extname(FISP3009ds)
022500080430     d                                          prefix(P : 3)
022600110202       // ·?10ª riga?
022700080430     d ds_10         e ds                  inz  extname(FISP3010ds)
022800080430     d                                          prefix(P : 3)
022900110202       // ·?11ª riga?
023000080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023100080430     d                                          prefix(P : 3)
023200110202       // ·?12ª riga?
023300080430     d ds_12         e ds                  inz  extname(FISP3012ds)
023400080430     d                                          prefix(P : 3)
023500110202       // ·?13ª riga?
023600080430     d ds_13         e ds                  inz  extname(FISP3013ds)
023700080430     d                                          prefix(P : 3)
023800110202       // ·?14ª riga?
023900080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024000080430     d                                          prefix(P : 3)
024100130205     d ds_140        e ds                  inz  extname(FISP30140s)
024200130205     d                                          prefix(P : 3)
024300110202       // ·?15ª riga?
024400080430     d ds_15         e ds                  inz  extname(FISP3015ds)
024500080430     d                                          prefix(P : 3)
024600110202       // ·?16ª riga?
024700080430     d ds_16         e ds                  inz  extname(FISP3016ds)
024800080430     d                                          prefix(P : 3)
024900110202       // ·?17ª riga?
025000080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025100080430     d                                          prefix(P : 3)
025200110202       // ·?18ª riga?
025300080430     d ds_18         e ds                  inz  extname(FISP3018ds)
025400080430     d                                          prefix(P : 3)
025500130205     d ds_180        e ds                  inz  extname(FISP30180s)
025600130205     d                                          prefix(P : 3)
025700110202       // ·?19ª riga?
025800080430     d ds_19         e ds                  inz  extname(FISP3019ds)
025900080430     d                                          prefix(P : 3)
026000110202       // ·?20ª riga?
026100080430     d ds_20         e ds                  inz  extname(FISP3020ds)
026200080430     d                                          prefix(P : 3)
026300110202       // ·?21ª riga?
026400080430     d ds_21         e ds                  inz  extname(FISP3021ds)
026500080430     d                                          prefix(P : 3)
026600110202       // ·?22ª riga?
026700080430     d ds_22         e ds                  inz  extname(FISP3022ds)
026800080430     d                                          prefix(P : 3)
026900110202       // ·?23ª riga?
027000080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027100080430     d                                          prefix(P : 3)
027200110202       // ·?24ª riga?
027300080430     d ds_24         e ds                  inz  extname(FISP3024ds)
027400080430     d                                          prefix(P : 3)
027500110202       // ·?25ª riga?
027600080430     d ds_25         e ds                  inz  extname(FISP3025ds)
027700080430     d                                          prefix(P : 3)
027800110202       // ·?26ª riga (vuota)?
027900110202       // ·?27ª riga?
028000080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028100080430     d                                          prefix(P : 3)
028200110202       // ·?28ª riga?
028300080430     d ds_28         e ds                  inz  extname(FISP3028ds)
028400080430     d                                          prefix(P : 3)
028500110202       // ·?29ª riga?
028600081021     d ds_29         e ds                  inz  extname(FISP3029ds)
028700081021     d                                          prefix(P : 3)
028800110202       // ·?30ª riga?
028900081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029000081021     d                                          prefix(P : 3)
029100110215       // ·?31ª riga?
029200110215     d ds_31         e ds                  inz  extname(FISP3031ds)
029300110215     d                                          prefix(P : 3)
029400110202       // ·?32ª riga?
029500081021     d ds_32         e ds                  inz  extname(FISP3032ds)
029600081021     d                                          prefix(P : 3)
029700110202       // ·?33ª riga?
029800081021     d ds_33         e ds                  inz  extname(FISP3033ds)
029900081021     d                                          prefix(P : 3)
030000110202       // ·?34ª riga?
030100081021     d ds_34         e ds                  inz  extname(FISP3034ds)
030200081021     d                                          prefix(P : 3)
030300110202       // ·?35ª riga (vuota)?
030400110202       // ·?36ª riga (vuota)?
030500110202       // ·?37ª riga e successive?
030600081023     d ds_37         e ds                  inz  extname(FISP3037ds)
030700081021     d                                          prefix(P : 3)
030800110202       // ·?Righe x FedEx?
030900081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031000081030     d                                          prefix(P : 3)
031100130411
031200130411       // -?Flag operativi in TITAS30C?
031300130411     d dTASflo       e ds                  inz     qualified
031400120702
031500130214       // -?Dati aggiuntivi in FIILV00F?
031600120702     d dILVlibero    e ds                  inz     qualified
031700101220
031800101220       // -?Parametri per conversione importi in Euro?
031900061107     d YEURCOds      e ds                  inz
032000110202
032100101220       // -?Dati da AZORG00F?
032200070424     d OG143ccm      e ds                  inz     extname(OG143)
032300070424     d                                             prefix(§CM:3)
032400070424     d OG146arr      e ds                  inz     extname(OG146)
032500070424     d                                             prefix(A)
032600070424     d OG146cf£6     e ds                  inz     extname(OG146)
032700070424     d                                             prefix(£6:1)
032800070424     d OG148arr      e ds                  inz     extname(OG148)
032900070424     d                                             prefix(A)
033000070424     d OG148cf£6     e ds                  inz     extname(OG148)
033100070424     d                                             prefix(£6:1)
033200070424     d OG150arr      e ds                  inz     extname(OG150)
033300070424     d                                             prefix(A)
033400110202
033500101220       // -?Dati da FIAR400F?
033600061108     d dsBL4A        e ds                  inz
033700061108     d dsBL4I        e ds                  inz
033800071015     d dsBL4M        e ds                  inz
033900121018     d dTA4Q         e ds                  inz
034000110228
034100110228       // -?Dati da FIDST00F?
034200110228     d dDSTflr       e ds                  inz
034300110202
034400101220       // -?Tabelle?
034500131206     d ds$2          e ds
034600061107     d ds1A          e ds                  inz
034700061107     d ds1P          e ds                  inz
034800070306     d ds2A          e ds                  inz
034900061107     d ds3A          e ds                  inz
035000061107     d ds5E          e ds                  inz
035100061107     d dsCC          e ds
035200061107     d dsCV          e ds                  inz
035300061107     d dsEI          e ds                  inz
035400061107     d dsTB          e ds                  inz
035500061107      *
035600030515     d TIBS02ds      e ds                  inz
035700040607     d   T02mod      e                     inz('C')
035800030805     d dGED          e ds
035900130206     d dILV          e ds
036000061031     d dVPOdeco      e ds                  inz
036100031124     d dAR5gen       e ds                  inz
036200061030     d dAR5bnb       e ds                  inz
036300080724
036400131210       // -?Flag operativi di FNEVB - campo EVBNOT?
036500131206     d dEVB01        e ds                  inz     qualified
036600101220       // -?Flag operativi di FNORM - campo ORMFLO?
036700100517     d dORM01        e ds                  inz
036800101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
036900030515     d TRUL21ds      e ds                  inz
037000120702     d   I21tla      e                     inz('L')
037100101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
037200070212     d FNLV55ds      e ds                  inz
037300120702     d   D55tpt      e                     inz('6')
037400101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
037500051129     d FNLV80ds      e ds                  inz
037600120702     d   ILV80tla    e                     inz('C')
037700101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
037800090619     d TNSD99ds      e ds                  inz
037900120702     d   D98tla      e                     inz('C')
038000130227
038100130227       // -?Tab. "MRA" = Bart-Mailing?
038200130227     d dMRAdan       e ds                  inz
038300130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
038400130227     d                                             ')
038500130227     d   §MRAdreg    e                     inz('COR')
038600130227     d   §MRAdmitt   e                     inz(c_Mittente)
038700130227     d   §MRAddest   e                     inz('stefano.merighi+
038800130227     d                                          @brt.it;')
038900130227     d   §MRAdidpro  e                     inz('1')
039000130227     d   §MRAdoutqi  e                     inz('EMAILIN')
039100130227
039200130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
039300130227     d TRTCM1ds      e ds                  inz
039400130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
039500130227     d   §CM1mitt    e                     inz(c_Mittente)
039600130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
039700130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
039800130716         //?·§CM1tips = Tipo lettera via e-mail:?
039900130716         // ?"LET" = testo allegato in corpo con logo?
040000130716         //         ?(richiede righe libere iniziali per il logo)?
040100130716         // ?"COR" = testo integrato senza logo?
040200130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
040300130716         // ?"SP1" = spool allegato (in formato PDF)?
040400130227     d   §CM1tips    e                     inz('COR')
040500130716         //?·§CM1po   = Sede?
040600130227     d   §CM1po      e                     inz(c_Sede)
040700130716         //?·§CM1var  = Oggetto e-mail?
040800130227     d   §CM1var     e                     inz('*OBJM*+
040900130227     d                                     Sped. elaborata oltre 100 +
041000130227     d                                     volte in FIILV - vedi log +
041100130227     d                                     "01"')
041200130716         //?·§CM1sts  = Stato?
041300130227     d   §CM1sts     e                     inz(*off)
041400130716         //?·§CM1idp  = Id processo?
041500130227     d   §CM1idp     e                     inz('2')
041600101220
041700101220       // -?Status ds?
041800030515     d Status         sds
041900120702     d   SDSpgm          *proc
042000120702     d   SDSprm          *parms
042100120702     d   SDSusr              254    263
042200101220
042300101220       // -?Parametro Numero Distinta?
042400101220     d ds_Distinta     ds             9    inz  qualified
042500101220     d   P_Fgs                        3s 0 inz
042600101220     d   P_Ndc                        6s 0 inz
042700080430
042800101207       // -?Parametro Numero Spedizione?
042900110207     d ds_Spedizione   ds            16    inz  qualified
043000080430     d   P_Aas                        4s 0 inz
043100080430     d   P_Lnp                        3s 0 inz
043200080430     d   P_Nrs                        2s 0 inz
043300080430     d   P_Nsp                        7s 0 inz
043400101220
043500080519     d BARcodeDS       ds            17    inz
043600120702     d   BARcdeAAS                    2s 0 inz
043700120702     d   BARcdeLNP                    3s 0 inz
043800120702     d   BARcdeNRS                    2s 0 inz
043900120702     d   BARcdeNSP                    7s 0 inz
044000101220
044100030515     d KEYpar          ds             8    inz
044200120702     d   WTPpar                1      1    inz
044300120702     d   WCDpar                2      3    inz
044400120702     d   Wprogr                4      8    inz('S0001')
044500101220
044600140303     d Elem_CP         ds                  inz
044700120702     d   WCPdcm                       8s 0 inz
044800140303     d   WCPocm                       4s 0 inz
044900120702     d   WCPncl                       5s 0 inz
045000140303     d   WCPfir                            inz  like(EVB2dati)
045100101213
045200101213       //--------------------------------------------------------------
045300101213       //?Definizione variabili globali.                               ?
045400101213       //--------------------------------------------------------------
045500101213
045600101220       // -?Parametri?
045700080430     d Spedizione      s             16
045800101217     d Distinta        s              9
045900101217     d DataOraCrea     s             14
046000080430     d Risultato       s              2
046100080715     d RootFolder      s            100
046200080519     d FileName        s             30
046300080731     d TemplateName    s             20
046400110201     d LastRow         s            120
046500131016     d RtnEsito        s             10i 0                                      // -?NON usato?
046600131016     d RtnOpCode       s             10                                         // -?NON usato?
046700131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
046800080430     d P_Prt           s              1
046900101220
047000130214       // -?Flags booleani?
047100101220     d $Mamma          s               n   inz
047200130215     d $Mamma_FedEx    s               n   inz
047300101220     d $FIAR8          s               n   inz
047400130411     d $§PDF           s               n   inz
047500110204     d $FIILV          s               n   inz
047600101220     d $NoDesPartic    s               n   inz
047700110117     d*// $BolloVirt      s               n   inz(*off)
047800120113     d $No_x_Cons      s               n   inz
047900101220     d $ConsParz       s               n   inz
048000101220     d $CAeur          s               n   inz
048100100517     d $PreavvORM      s               n   inz
048200101220     d $GeoAtt         s               n   inz
048300131206     d $1o_Passaggio   s               n   inz
048400101220
048500101220       // -?Indici di schiera?
048600080523     d XX              s              3  0 inz
048700140304     d YY              s              3  0 inz
048800080523     d WX              s              3  0 inz
048900130313
049000130313       // -?Nome esteso Libreria/File del file TITAS30C?
049100130313     d wLibFileTITAS   s             21a   inz
049200080724
049300101207       // -?Campi di stampa?
049400130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
049500130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
049600101220
049700101220       // -?Campi di comodo alfanumerici?
049800030717     d w_CoOr          s             35    inz
049900081030     d wAWB            s             12    inz
050000030515     d w001a           s              1    inz
050100030515     d w008a           s              8    inz
050200030515     d w015a           s             15    inz
050300100517     d w053a           s             53    inz
050400070424     d wDateA          s              8    inz
050500070717     d wGeoTel         s              1    inz
050600070717     d wGeoCon         s              1    inz
050700080718     d wSysName        s              8    inz
050800130411      *? x TEST ?
050900130411     d $Test           s              3    inz
051000101220
051100101220       // -?Campi di comodo numerici?
051200030515     d w0080           s              8  0 inz
051300030624     d w0112           s             11  2 inz
051400030515     d w0140           s             14  0 inz
051500071105     d wDate           s              8  0 inz
051600090120     d w_Row           s              3  0 inz(c_Row)
051700110309     d wCountRec01     s              5  0 inz
051800101220
051900131008       // -?Campi utilizzati per impostazione dell'output?
052000110117     d*// wBolloVirt      s              4  2 inz
052100030515     d Wcas            s             13  3 inz
052200030515     d Wiva            s             11  2 inz
052300071015     d Wpdc4A          s              4    inz
052400030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
052500090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
052600101220
052700101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
052800040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
052900121018     d wB0cfi          s                   like(DB0cpi)   inz
053000121018     d wB0piv          s                   like(DB0cpi)   inz
053100120702     d wB0nrs          s                   like(ARBnrs)   inz
053200070514     d wB0ntc          s                   like(ARBntc)   inz
053300080523     d wB0dcm          s                   like(ARBdcm)   inz
053400070514     d wB0dcp          s                   like(ARBdcp)   inz
053500081120     d wB0tmc          s                   like(ARBtmc)   inz
053600130205     d wB0fiv          s                   like(AR6fiv)   inz
053700080506     d Wnsc            s                   like(ARTnsc)   inz
053800080506     d Wtrc            s                   like(AR4trc)   inz
053900030515     d Wctr            s                   like(DB0ctr)   inz
054000031124     d Wdsp            s                   like(DB0dft)   inz
054100090619     d WddtCLN         s                   like(DB0drt)   inz
054200090619     d WdspCLN         s              6  0                inz
054300030515     d Wsvx            s                   like(AR6sv1)   inz
054400030515     d Wvax            s                   like(AR6va1)   inz
054500110203     d wCod_Log        s                   like(ILVcol)   inz
054600090619
054700140303       //--------------------------------------------------------------
054800140303       //?Definizione procedure usate.                                 ?
054900090619       //--------------------------------------------------------------
055000100322
055100100322       // -?Procedure name: getCurrentSystemName?
055200100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
055300100322       //  ?Returns:        Current System Name?
055400130716       //  ?(vedi "QWCRNETA")?
055500100322     d getCurrentSystemName...
055600100322     d                 pr             8a
055700100322
055800100322       //--------------------------------------------------------------
055900140303       //?Definizione procedure esterne.                               ?
056000100322       //--------------------------------------------------------------
056100100322
056200110201       // -?Calcolo check digit del barcode?
056300110201     d TRUL28ds      e ds                  inz
056400110201     d  I28tla       e                     inz('L')
056500110201     d  I28mod       e                     inz('BAR')
056600110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
056700110201     d  trul28ds                           likeds(TRUL28ds)
056800110201
056900100322       // -?Calcolo tempi di delivery?
057000100322      /copy gaitrasrc/srcProtoPR,TNSD99R
057100130206
057200130206       // -?Ricerca/Controllo tabelle?
057300130206      /copy gaitrasrc/srcProtoPR,TIBS02R
057400100322
057500110201       // -?Parametri per API "QWCRNETA"?
057600110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
057700110201     d qWcrNetA_dataReturned...
057800110201     d                 ds                  qualified inz
057900110201     d  qWcRDR                             likeds(qWCRDR)
058000110201     d  qWcOAT                       10i 0 dim(00001)
058100110201     d  qWcARRAY                     24
058200110201     d qWCRNETA_networkAttributeInformationTable...
058300110201     d                 ds                  qualified inz
058400110201     d  qWcRAT                             likeds(qWCRAT)
058500110201     d  qWcDATA                       8
058600100322       // -?Reperimento attributo di rete current System Name?
058700100322       //  ?(vedi "getCurrentSystemName")?
058800100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
058900130227
059000130227       // -?Parametri API QCAPCMD (Process Commands)?
059100130227     d Qcmd            s           2048    inz  varying
059200130227      /copy qSysInc/qRpgleSrc,QCAPCMD
059300130227       // -?API QCAPCMD (Process Commands)?
059400130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
059500130227
059600130227       // -?Parametri gestione errori API.?
059700130227      /copy qsysinc/qRpgleSrc,QUSEC
059800080710
059900080710       //--------------------------------------------------------------
060000140303       //?Definizione key-list.                                        ?
060100080710       //--------------------------------------------------------------
060200130313
060300130422       // -?File FIIRD11L?
060400130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
060500130313     d                                     prefix(k_)
060600130313     d                                     inz
060700130313
060800130313       // -?File TITAS30C?
060900130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061000130313     d                                     prefix(k_)
061100130313     d                                     inz
061200131206
061300131206       // -?File FNEVB04L?
061400131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
061500131206     d                                     prefix(k_)
061600131206     d                                     inz
061700140303
061800140303       // -?File FNEVB22L?
061900140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062000140303     d                                     prefix(k_)
062100140303     d                                     inz
062200080710
062300101207       // -?File FIDST01L?
062400110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
062500080710     d                                     prefix(k_)
062600080710     d                                     inz
062700090216
062800110202       // -?File FIILV01L?
062900110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
063000110207     d                                     prefix(k_)
063100090216     d                                     inz
063200030515
063300101220       //==============================================================
063400101220       //?RIEPILOGO INDICATORI                                         ?
063500101220       //--------------------------------------------------------------
063600101220       // 31    - di comodo
063700101220       // 35    - Porto Franco: segnala
063800101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
063900101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
064000101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
064100120524       // 70    - Trattasi di BOLLA BRT
064200101220       // 71    - Trattasi di BOLLA DPD
064300101220       // 72    - Trattasi di BOLLA FedEx
064400101220       // 73    - Trattasi di BOLLA EuroExpress
064500101220       //==============================================================
064600030515
064700101213       //--------------------------------------------------------------
064800101213       //?M A I N - L I N E                                            ?
064900101213       //--------------------------------------------------------------
065000101213
065100030515     c     *Entry        plist
065200080430     c                   parm                    Spedizione
065300101217     c                   parm                    Distinta
065400101217     c                   parm                    DataOraCrea
065500080430     c                   parm                    Risultato
065600080715     c                   parm                    RootFolder
065700080519     c                   parm                    FileName
065800080731     c                   parm                    TemplateName
065900110201     c                   parm                    LastRow
066000080430     c                   parm                    RtnEsito
066100080430     c                   parm                    RtnOpCode
066200080430     c                   parm                    RtnStatus
066300080430     c                   parm                    P_Prt
066400080430      *
066500110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
066600110519      *       ?chiamato da menù AS400 x test?
066700101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
066800080430      *
066900070123      *
067000130924      * Operazioni iniziali
067100131016       //?SE rilevati errori nei parametri di input =>?
067200131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
067300070123     c                   exsr      sr_Start
067400030910      *
067500130924      * Pulizia dei campi di output e/o di lavoro
067600130924     c                   exsr      sr_ClrFLD
067700030515      *
067800130924      * Caricamento dei dati per l'immagine della LdV
067900131016       //?SE rileva errori nella distinta o nella spedizione =>?
068000131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068100110208     c                   exsr      sr_CarARB
068200030515      *
068300130924      * Controllo del tipo LDV di cui generare l'immagine
068400110208     c                   exsr      sr_TipoLDV
068500030515      *
068600080430      * Impostazione dei campi di output con quelli della DS
068700110208     c                   exsr      sr_PrepPRT
068800030515      *
068900080430      * Decodifica dei dati della LDV
069000110208     c                   exsr      sr_DecodLDV
069100080806      *
069200080806      * Impostazione dati per immagine LdV
069300131016     c                   exsr      sr_DatiOUT
069400030515      *
069500030515      * Chiusura pgm
069600080430     c                   exsr      sr_End
069700030515      *
069800030515      *---------------------------------------------------------------*
069900050217      *?OPERAZIONI INIZIALI                                          ?*
070000030515      *---------------------------------------------------------------*
070100030515     c     *InzSR        BEGSR
070200070424      *
070300070424      * Reperisco la data del giorno
070400070424     c                   time                    w0140
070500070424     c                   move      w0140         w0080
070600070424     c     *eur          movel     w0080         Data_EUR
070700070424     c     *iso          movel     Data_EUR      w0080
070800070424     c                   move      w0080         wDateA
070900071105     c                   eval      wDate  = w0080
071000030515      *
071100030515      * Imposto i campi chiave "fissi"
071200040223     c                   eval      TBLkut = 1
071300100901     c                   eval      ARStrc = 'C'
071400030515      *
071500040617      * Impostazione dati in KPJBA
071600070308     c                   eval      KNMUS  = SDSusr
071700030515      *
071800030515      * Reperisco la moneta di conto
071900030515     c                   clear                   dGED
072000040607     c                   clear                   TIBS02ds
072100040607     c                   eval      T02mod = 'C'
072200040607     c                   eval      T02cod = 'GED'
072300040607     c                   eval      T02ke1 = '1         '
072400030515     c                   call      'TIBS02R'
072500030515     c                   parm                    KPJBA
072600040607     c                   parm                    TIBS02ds
072700030515if  1c                   if        T02err = *blanks
072800030515     c                   movel     T02uni        dGED
072900030515e   1c                   endif
073000061031      *
073100131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
073200131008      *                                        senza riserve"
073300131008      *              e di decorrenza indicaz. "Consegna al mattino"
073400061031     c                   clear                   dVPOdeco
073500061031     c                   clear                   TIBS02ds
073600061031     c                   eval      T02mod = 'C'
073700071112     c                   eval      T02cod = 'VPO'
073800061031     c                   eval      T02ke1 = 'DECO      '
073900061031     c                   call      'TIBS02R'
074000061031     c                   parm                    KPJBA
074100061031     c                   parm                    TIBS02ds
074200061031if  1c                   if        T02err = *blanks
074300061031     c                   movel     T02uni        dVPOdeco
074400061031e   1c                   endif
074500030515      *
074600030515     c                   ENDSR
074700070123      *
074800101220      /free
074900101220
075000101220       //--------------------------------------------------------------
075100101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
075200101220       //--------------------------------------------------------------
075300101220       BEGSR  sr_Start;
075400101220
075500101220         exec sql  set option  dynusrprf = *owner,
075600101220                               closqlcsr = *endmod;
075700101220
075800101221         *inLR = *on;
075900080509
076000110517         // -?Impostazione iniziale dei dati di ritorno?
076100080509         Risultato = 'OK';
076200110202         clear  RootFolder;
076300110202         clear  FileName;
076400120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
076500120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
076600130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
076700110202         clear  LastRow;
076800110202         clear  RtnEsito;
076900110202         clear  RtnOpCode;
077000110202         clear  RtnStatus;
077100110207
077200110207         // -?Apertura archivio di output - comunque utilizzato?
077300110207         if  NOT  %open(FIILV01L);
077400110207           open(e)  FIILV01L;
077500110207           if  %error;
077600110207             exsr  sr_ErrOpenFile;
077700110207           endif;
077800110207         endif;
077900110207
078000110202         clear  k06fiilv01;
078100110203         clear  FIILV000;
078200080509
078300110203         // »? STEP 0 ?«
078400131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
078500131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
078600130716         exsr  sr_STEP_0_CtrlParms;
078700110203
078800080430
078900101213         // -?Impostazione spedizione ricevuta come parametro?
079000080430         ds_Spedizione = Spedizione;
079100110126         // -?Impostazione distinta ricevuta come parametro?
079200110126         ds_Distinta = Distinta;
079300080430
079400101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
079500130924         //clear FNLSB5ds;            ?(già così)?
079600080430         DB0tbo = 'A';
079700110202         DB0aas = ds_Spedizione.P_Aas;
079800110202         DB0lnp = ds_Spedizione.P_Lnp;
079900110202         DB0nrs = ds_Spedizione.P_Nrs;
080000110202         DB0nsp = ds_Spedizione.P_Nsp;
080100080519
080200101213         // -?Impostazione campo di output "identificativo spedizione"?
080300080519         clear  BarCodeDS;
080400080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
080500080519         BARcdeLNP = DB0lnp;
080600080519         BARcdeNRS = DB0nrs;
080700080519         BARcdeNSP = DB0nsp;
080800080519         FileName  = BarCodeDS;
080900130313
081000130716         // -?Impostazione chiave di accesso al file FIIRD11L?
081100130313         k_IRDfgsndc = Distinta;
081200130313
081300130313         // -?Impostazione chiave di accesso al file TITAS30C?
081400130313         clear  k05titas30;
081500130313         k_TASaas = DB0aas;
081600130313         k_TASlnp = DB0lnp;
081700130313         k_TASnrs = DB0nrs;
081800130313         k_TASnsp = DB0nsp;
081900131206
082000131206         // -?Impostazione chiave di accesso al file FNEVB04L?
082100131206         clear  keyFNEVB04;
082200131206         k_EVBaas = DB0aas;
082300131206         k_EVBlnp = DB0lnp;
082400131206         k_EVBnrs = DB0nrs;
082500131206         k_EVBnsp = DB0nsp;
082600131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
082700140303
082800140303         // -?Impostazione chiave di accesso al file FNEVB22L?
082900140303         clear  keyFNEVB22;
083000140303         k_EVB2aas = DB0aas;
083100140303         k_EVB2lnp = DB0lnp;
083200140303         k_EVB2nrs = DB0nrs;
083300140303         k_EVB2nsp = DB0nsp;
083400140303         //k_EVB2dtv = .....;
083500140303         //k_EVB2orv = .....;
083600140303         //k_EVB2cev = 'P  ';
083700110204
083800110204         // -?Impostazione chiave di accesso al file FIILV01L?
083900110204         k_ILVaas = DB0aas;
084000110204         k_ILVlnp = DB0lnp;
084100110204         k_ILVnrs = DB0nrs;
084200110204         k_ILVnsp = DB0nsp;
084300110225         k_ILVfgs = ds_Distinta.p_Fgs;
084400110225         k_ILVndc = ds_Distinta.p_Ndc;
084500120702         clear  dILVlibero;
084600130411
084700130411
084800130411         IF  '1' <> '1';
084900130411
085000130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
085100130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
085200130924           clear  $Test;
085300130411           Select;
085400130411             // -?Spedizione FedEx?
085500130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
085600130411               $Test = 'FED';
085700130411             // -?Spedizione DPD?
085800130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
085900130411               $Test = 'DPD';
086000130411             // -?Spedizione con Consegna Parziale Precedente?
086100130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
086200130411               $Test = 'C_P';
086300130411             // -?Spedizione Normale?
086400130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
086500130411               $Test = 'NOR';
086600130411             // -?Altra spedizione: NON ELABORABILE?
086700130411             Other;
086800130411               Risultato = 'NO';
086900130924               //RtnOpCode = 'NO_IN_TEST';
087000130924               //RtnEsito  = -1;
087100130924               // -?Uscita dalla s.p.?
087200130411               exsr  sr_End;
087300130411           EndSl;
087400130411
087500130411         ENDIF;
087600130411
087700080430
087800110208         // -?Apertura archivi di input - comunque utilizzati?
087900110207         if  NOT  %open(TABEL00F);
088000110207           open(e)  TABEL00F;
088100110207           if  %error;
088200110207             exsr  sr_ErrOpenFile;
088300110207           endif;
088400110207         endif;
088500110207
088600110207         if  NOT  %open(AZORG01L);
088700110207           open(e)  AZORG01L;
088800110207           if  %error;
088900110207             exsr  sr_ErrOpenFile;
089000110207           endif;
089100110207         endif;
089200110207
089300110207         if  NOT  %open(FNARB01L);
089400110207           open(e)  FNARB01L;
089500110207           if  %error;
089600110207             exsr  sr_ErrOpenFile;
089700110207           endif;
089800110207         endif;
089900110207         if  NOT  %open(FIAR601L);
090000110207           open(e)  FIAR601L;
090100110207           if  %error;
090200110207             exsr  sr_ErrOpenFile;
090300110207           endif;
090400110207         endif;
090500110207         if  NOT  %open(FIAR701L);
090600110207           open(e)  FIAR701L;
090700110207           if  %error;
090800110207             exsr  sr_ErrOpenFile;
090900110207           endif;
091000110207         endif;
091100110207         if  NOT  %open(FIAR901L);
091200110207           open(e)  FIAR901L;
091300110207           if  %error;
091400110207             exsr  sr_ErrOpenFile;
091500110207           endif;
091600110207         endif;
091700110207         if  NOT  %open(FIDST01L);
091800110207           open(e)  FIDST01L;
091900110207           if  %error;
092000110207             exsr  sr_ErrOpenFile;
092100110207           endif;
092200110207         endif;
092300101213
092400101213         if  NOT  %open(FIAR401L);
092500101213           open(e)  FIAR401L;
092600101213           if  %error;
092700101213             exsr  sr_ErrOpenFile;
092800101213           endif;
092900101213         endif;
093000101213         if  NOT  %open(FIAR501L);
093100101213           open(e)  FIAR501L;
093200101213           if  %error;
093300101213             exsr  sr_ErrOpenFile;
093400130411           endif;
093500130411         endif;
093600101213
093700101220       ENDSR;
093800101220
093900101213       //--------------------------------------------------------------
094000101213       //?Restituzione errore in fase di apertura di un file           ?
094100101213       //--------------------------------------------------------------
094200101213       BEGSR  sr_ErrOpenFile;
094300101213
094400101213         Risultato = 'NO';
094500130924         //RtnOpCode = 'ERR_OPEN  ';
094600101213         //RtnEsito  = -1;
094700101213
094800130924         // -?Uscita dalla s.p.?
094900101213         exsr  sr_End;
095000101213
095100101213       ENDSR;
095200130716
095300130716       //--------------------------------------------------------------
095400130716       //?»  STEP 0  «                                                 ?
095500130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
095600130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
095700130716       //--------------------------------------------------------------
095800130716       BEGSR  sr_STEP_0_CtrlParms;
095900130716
096000130716         //   _________________________________________________________
096100130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
096200130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
096300130716         SELECT;
096400130716
096500130716           // -?NUMERO SPEDIZIONE?
096600130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
096700130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
096800130716                                      Spedizione   <= *zero;
096900130716             Risultato = 'XX';
097000130924             //RtnOpCode = 'ERR_NUMSPE';
097100130716             //RtnEsito  = -1;
097200130716             // -?Registrazione log in FIILV?
097300130716             wCod_Log = '21';
097400130716             exsr  sr_WrtFIILV;
097500130716             // -?Uscita dalla s.p.?
097600130716             exsr  sr_End;
097700130716
097800130716           // -?NUMERO DISTINTA?
097900130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
098000130716                 ( %check (c_Digits : Distinta) ) > *zero  or
098100130716                                      Distinta   <= *zero;
098200130716             Risultato = 'XX';
098300130924             //RtnOpCode = 'ERR_DISTIN';
098400130716             //RtnEsito  = -1;
098500130716             // -?Registrazione log in FIILV?
098600130716             wCod_Log = '21';
098700130716             exsr  sr_WrtFIILV;
098800130716             // -?Uscita dalla s.p.?
098900130716             exsr  sr_End;
099000130716         ENDSL;
099100130716
099200130716         // ________________________________________________________
099300130716         //? Reperimento nome del sistema per impostazione percorso ?
099400130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
099500130716         wSysName = getCurrentSystemName();
099600130716         if  wSysName = *blank;
099700130716           Risultato  = 'NO';
099800130924           //RtnOpCode  = 'SYS_ERR   ';
099900130924           //RtnEsito   = -1;
100000130924           // -?Uscita dalla s.p.?
100100130716           exsr  sr_End;
100200130716         endif;
100300130716
100400130716         // ______________________________________________________
100500130716         //? Reperimento dei percorsi in base al nome del sistema ?
100600130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100700130716         clear  dILV;
100800130716         reset  TIBS02ds;
100900130716         T02cod = 'ILV';
101000130716         T02ke1 = %subst( wSysName : 1 : 6 );
101100130716         T02ke2 = 'LINEA';
101200130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
101300130716         if  T02err = *blank;
101400130716           dILV = T02uni;
101500130716         endif;
101600130716
101700130716       ENDSR;
101800110208
101900110208       //--------------------------------------------------------------
102000130924       //?Pulizia dei campi di output e/o di lavoro                    ?
102100110208       //--------------------------------------------------------------
102200130924       BEGSR  sr_ClrFLD;
102300080430
102400081021         clear ds_01;
102500101213       //clear ds_02;                      ?(riga vuota)?
102600080430         clear ds_03;
102700080430         clear ds_04;
102800080430         clear ds_05;
102900101213       //clear ds_06;                      ?(riga vuota)?
103000080430         clear ds_07;
103100101213       //clear ds_08;                      ?(riga vuota)?
103200080430         clear ds_09;
103300080430         clear ds_10;
103400080430         clear ds_11;
103500080430         clear ds_12;
103600080430         clear ds_13;
103700080430         clear ds_14;
103800130205         clear ds_140;
103900080430         clear ds_15;
104000080430         clear ds_16;
104100080430         clear ds_17;
104200080430         clear ds_18;
104300130205         clear ds_180;
104400080430         clear ds_19;
104500080430         clear ds_20;
104600080430         clear ds_21;
104700080430         clear ds_22;
104800080430         clear ds_23;
104900080430         clear ds_24;
105000080430         clear ds_25;
105100101213       //clear ds_26;                      ?(vuoto)?
105200080430         clear ds_27;
105300080430         clear ds_28;
105400081021         clear ds_29;
105500081021         clear ds_30;
105600110215         clear ds_31;
105700081021         clear ds_32;
105800081021         clear ds_33;
105900081021         clear ds_34;
106000081023         clear ds_37;
106100081030         clear ds_Fed;
106200080430
106300080430      /end-free
106400030515      *
106500060524     c                   clear                   dsBL4A
106600061030     c                   clear                   dsBL4I
106700110204     c                   clear                   dsBL4M
106800100517     c                   clear                   dOrm01
106900041207      *
107000070514     c                   reset                   $Mamma
107100130215     c                   reset                   $Mamma_FedEx
107200060921     c                   reset                   $FIAR8
107300130411     c                   reset                   $§PDF
107400110204     c                   reset                   $FIILV
107500030710     c                   reset                   $NoDesPartic
107600110117     c*//                reset                   $BolloVirt
107700040707     c                   reset                   $No_x_Cons
107800070319     c                   reset                   $ConsParz
107900030515     c                   reset                   $CAeur
108000100901     c                   reset                   $PreavvORM
108100101220     c                   reset                   $GeoAtt
108200070717     c                   clear                   wGeoTel
108300070717     c                   clear                   wGeoCon
108400070717     c                   clear                   Wdsp
108500090619     c                   clear                   WddtCLN
108600090619     c                   clear                   WdspCLN
108700030717     c                   clear                   W_CoOr
108800110117     c*//                clear                   wBolloVirt
108900030515     c                   clear                   Wcas
109000030515     c                   clear                   Wiva
109100071015     c                   clear                   Wpdc4a
109200121018     c                   clear                   wB0cfi
109300121018     c                   clear                   wB0piv
109400031015     c                   clear                   wB0nrs
109500070514     c                   clear                   wB0ntc
109600080523     c                   clear                   wB0dcm
109700070514     c                   clear                   wB0dcp
109800081120     c                   clear                   wB0tmc
109900130205     c                   clear                   wB0fiv
110000030515      *
110100030515     c                   clear                   $NP
110200070319     c                   reset                   $CP
110300070319     c                   reset                   $CPdcm
110400030515      *
110500030515     c                   movea     *zeros        *in(01)
110600030515      *
110700030515     c                   ENDSR
110800030515      *
110900030515      *---------------------------------------------------------------*
111000131008      *?Determino il modulo LDV da usare per l'immagine              ?*
111100131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
111200131008      *? promrmoria per l'autotrasportatore).                        ?*
111300030515      *---------------------------------------------------------------*
111400110208     c     sr_TipoLDV    BEGSR
111500070215      *
111600070212      * Impostazione della data spedizione
111700110208      * (se non già impostata in subr. "sr_CarARB")
111800070212if  1c                   if        Wdsp   =  *zeros
111900070212if  2c                   if        DB0mgs <> *zeros
112000070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
112100070212x   2c                   else
112200070212      *   prendo la data del giorno se non c'è la data spedizione
112300070212     c                   time                    w0140
112400070212     c                   move      w0140         w0080
112500070212     c     *eur          movel     w0080         Data_EUR
112600070212     c     *iso          movel     Data_EUR      Wdsp
112700070212e   2c                   endif
112800070212e   1c                   endif
112900040607      *
113000070308      * Dati della filiale del mittente
113100040607     c                   eval      WX     =  DB0ccm / 10000
113200040607if  1c                   if        WX     <> ORGfil
113300040607     c     WX            chain     AZORG
113400040607e   1c                   endif
113500040607if  1c                   if        %found(AZORG01L)
113600040607     c                   movel     ORGde3        OG143ccm
113700040607x   1c                   else
113800040607     c                   clear                   OG143ccm
113900040607e   1c                   endif
114000040311      *
114100040311      * Dati della linea di arrivo
114200131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
114300131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
114400070212      *
114500070212      * Dati della capofila della linea di arrivo
114600131205      * - per verificare TELLUS
114700070717      *
114800070717      *** Imposta flag attivazione GEO x LNA
114900070717     c                   eval      wGeoCon = A§OGtlls
115000070717     c                   eval      wGeoTel = A§OGgeot
115100070717if  1c                   if             A§OGdda  > *zeros
115200070717     c                             and  A§OGdda  < C_DateMax_A
115300070717     c                             and  A§OGdda <= wDateA
115400101220     c                   eval      $GeoAtt = *on
115500070717e   1c                   endif
115600070717      *
115700110203if  1c                   IF             wGeoCon <> 'S'
115800070717     c                              or  wGeoTel <> 'S'
115900110203     c                              or  $GeoAtt <> *on
116000070212      * - Reperimento capofila (£6)
116100070212if  2c                   if        DB0lna <> D55lin
116200070212     c                   reset                   FNLV55ds
116300130924     c***?già così:?     eval      D55tpt =  '6'
116400070212     c                   eval      D55lin =  DB0lna
116500070212     c                   eval      D55drf =  Wdsp
116600070212     c                   call      'FNLV55R'
116700070212     c                   parm                    FNLV55ds
116800070212if  3c                   if        D55err <> *blanks
116900070212     c                   eval      D55tfa =  DB0lna
117000070212e   3c                   endif
117100070212e   2c                   endif
117200070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
117300070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
117400070212if  2c                   if        D55tfa <> ORGfil
117500070212     c     D55tfa        chain     AZORG
117600071109if  3c                   if        %found(AZORG01L)
117700070717     c                   movel     ORGde6        OG146cf£6
117800070717     c                   movel     ORGde8        OG148cf£6
117900071109x   3c                   else
118000071109     c                   clear                   OG146cf£6
118100071109     c                   clear                   OG148cf£6
118200071109e   3c                   endif
118300071109e   2c                   endif
118400070717      *** Imposta flag attivazione GEO x capofila £6
118500070717     c                   eval      wGeoCon = £6OGtlls
118600070717     c                   eval      wGeoTel = £6OGgeot
118700071109if  2c                   if             £6OGdda  > *zeros
118800070717     c                             and  £6OGdda  < C_DateMax_A
118900070717     c                             and  £6OGdda <= wDateA
119000101220     c                   eval      $GeoAtt = *on
119100071109e   2c                   endif
119200070717      *
119300070212e   1c                   ENDIF
119400040607      *
119500130411      * Reperimento dati da FIAR5 / "GEN"
119600130411      *   (per verificare se esistono Packing-List e/o PDF allegati
119700130411      *    - oltre che per reperire Nominativo e Telefono)
119800130411     c                   exsr      RepAR5gen
119900130411      *
120000130411      * Verifica se LDV con DDT e/o PDF del mittente
120100130411     c                   exsr      CercaDDToPDF
120200131206      *
120300131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
120400131206     c                   exsr      sr_Check1pass
120500070319      *
120600070319      * Verifica se richiesta spedizione già parzialmente consegnata
120700070319      *   (da stampare nel box "Note per l'autotrasportatore")
120800070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
120900070717      * completamente
121000110203if  1c                   if             wB0dcp   >  *zeros
121100101220     c                             and  $GeoAtt  =  *on
121200070413     c                   eval      $ConsParz     =  *on
121300070319e   1c                   endif
121400030515      *
121500040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
121600040607      *   riserve)
121700120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
121800040223     c                   movea     '1000'        *in(70)
121900040311      *
122000030515sel 1c                   SELECT
122100071213      *
122200131203      * LDV DPD (dalla fil. del mittente, NON della LNP)
122300071213w   1c                   WHEN      §CMntw = 'DPD'
122400071213     c                   movea     '0100'        *in(70)
122500071213      *
122600071213      * LDV con mittente FedEx
122700150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
122800150430     c                             DB0ccm = c_CliFedEx2
122900071213     c                   movea     '0010'        *in(70)
123000030515      *
123100030515e   1c                   ENDSL
123200040707      *
123300040707      * Controllo se devo stampare "Non utilizzare per la consegna"
123400040707      * nella copia per il vettore,?senza?copia per il destinatario:
123500130924     c***?(già fatto:)?  reset                   $No_x_Cons
123600071203      * - LdV recapito c/assegni:
123700100322      *   stampata in arrivo e con
123800120113if  1c                   if             DB0lnp =  102
123900070306     c                             and  DB0cbo =  'FW'
124000071203     c                             and  DB0nft =  *zeros
124100071203      *   - fil. arrivo con GEO attivato
124200071203     c                             and (DB0fl1    =  'A'
124300071203     c                              or  wGeoCon   =  'S'
124400071203      *   - fil. arrivo abilitata al picking
124500120113     c                              or  A§OGpck   =  'S')
124600070306     c                   eval      $No_x_Cons  =  *on
124700120113e   1c                   endif
124800030515      *
124900030515     c                   ENDSR
125000040607      *
125100040607      *---------------------------------------------------------------*
125200130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
125300130411      *?               di uno o più PDF allegati da stampare         ?*
125400040607      *---------------------------------------------------------------*
125500130411     c     CercaDDToPDF  BEGSR
125600040607      *
125700130924     c***?(già fatto:)?  reset                   $FIAR8
125800130924     c***?(già fatto:)?  reset                   $§PDF
125900040607      *
126000130924     c***?(già fatto:)?  exsr      RepAR5gen
126100130411      *
126200130411      * -?Verifica esistenza PDF allegato?
126300130411     c                   eval      $§PDF  = (§AR5als = 'S')
126400130411      *
126500130411      * -?Verifica esistenza packing-list?
126600130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
126700040607      *
126800080505     c                   ENDSR
126900131206      *
127000131206      /free
127100131206
127200131206       //--------------------------------------------------------------
127300131206       //?Verifica se già effettuato il 1° passaggio.                  ?
127400131206       //--------------------------------------------------------------
127500131206       BEGSR  sr_Check1pass;
127600131206
127700131206         //reset  $1o_Passaggio       ?(già fatto)?
127800131206
127900131206         // -?Apertura archivio?
128000131206         If  NOT  %open(FNEVB04L);
128100131206           open(e)  FNEVB04L;
128200131206           if  %error;
128300131206             exsr  sr_ErrOpenFile;
128400131206           endif;
128500131206         EndIf;
128600131206
128700131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
128800131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
128900131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
129000131206
129100131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
129200131206
129300131206           dEVB01 = EVBnot;
129400131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
129500131206
129600131206           if  Not $1o_Passaggio;
129700131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
129800131206           endif;
129900131206
130000131206         EndDo;
130100131206
130200131206       ENDSR;
130300131206
130400131206      /end-free
130500030515      *
130600030515      *---------------------------------------------------------------*
130700050217      *?Imposta i campi di stampa con quelli della DS                ?*
130800030515      *---------------------------------------------------------------*
130900110208     c     sr_PrepPRT    BEGSR
131000040223      *
131100040223      * Reperimento dati da FIAR5 / "GEN"
131200130924     c***?(già fatto:)?  exsr      RepAR5gen
131300070212     *** *
131400070212     *** * Impostazione Data spedizione
131500110208     *** * (già fatto nella subr. "sr_TipoLdV")
131600030515      *
131700030515      * - Intestazione moduli
131800030515      *
131900030515      * - Sigla operatore video
132000030515     c                   eval      Psop   =  DB0sop
132100081120      /free
132200081120         //
132300130411         // -?Flag "allega documento di trasporto"?
132400081120         select;
132500081120
132600130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
132700081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
132800081120             evalr Pddt =  'DDT sul collo';
132900081120
133000130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
133100081120           when  DB0bam =  'S';
133200081120             evalr Pddt =  'DDT';
133300081120
133400130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
133500130716           //// -?Esiste packing-list abbinata?
133600130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
133700131203           //  evalr Pddt =  'PACKING-LIST';
133800130716           //// -?LdV con PDF allegato?
133900130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
134000130411           //  evalr Pddt =  'PACKING-LIST PDF';
134100130716           //// -?LdV con packing-list abbinata & PDF allegato?
134200130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
134300130411           //  evalr Pddt =  'PACKING-LIST PDF';
134400130411
134500130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
134600081120           other;
134700081120             clear Pddt;
134800081120
134900081120         endsl;
135000081120
135100081120      /end-free
135200030515      *
135300030515      * - Mittente
135400030515     c                   if        DB0ccm <> *zeros
135500030515     c                   move      DB0ccm        Pccm
135600030515     c                   endif
135700030515     c                   eval      Prsm   =  DB0rsm
135800030515     c                   eval      Pinm   =  DB0inm
135900030515     c                   eval      Pcam   =  DB0cam
136000030515     c                   eval      Plom   =  DB0lom
136100030515     c                   eval      Pprm   =  DB0prm
136200030515     c                   eval      Pnzm   =  DB0nzm
136300030515      *   Mittente originale
136400030515if  1c                   if        DB0rmo <> *blanks
136500080430     c                   eval      Pkmo   =  'Mitt.Originale '
136600030515     c                   eval      Prmo   =  DB0rmo
136700030515e   1c                   endif
136800030515      *
136900070308      * - Filiale di Arrivo
137000030515     c                   move      DB0lna        Plna
137100030515      *
137200030515      * - Destinatario
137300030515if  1c                   if        DB0ccd <> *zeros
137400030515     c                   move      DB0ccd        Pccd
137500030515e   1c                   endif
137600030515     c                   eval      Prsd   =  DB0rsd
137700030515     c                   eval      Prse   =  DB0rd2
137800030515     c                   eval      Pind   =  DB0ind
137900030515     c                   eval      Pcad   =  DB0cad
138000030515     c                   eval      Plod   =  DB0lod
138100030515     c                   eval      Pprd   =  DB0prd
138200030515     c                   eval      Pnzd   =  DB0nzd
138300030515      *
138400030515      * - Codice tariffa
138500030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
138600030515      *   - nel box del destinatario se porto assegnato
138700030515     c                   move      DB0ctr        Pcta
138800030515x   1c                   else
138900030515      *   - nel box del mittente se porto franco
139000030515     c                   move      DB0ctr        Pctf
139100030515e   1c                   endif
139200030515      *
139300030515      * - Riferimenti mittente
139400080430     c                   evalr     Prmn   =  %char(DB0rmn)
139500030515     c                   eval      Prma   =  DB0rma
139600061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
139700130924     c***?(già fatto:)?  clear                   dsBL4I
139800060626     c                   if        *in71
139900061103     c                   exsr      RepAR4_I
140000060626     c                   endif
140100030515      *
140200030515      * - Nominativo & telefono
140300031124      *   (da FIAR5 / "GEN")
140400130924     c***?(già fatto:)?  exsr      RepAR5gen
140500031124     c                   eval      Pnom   =  §AR5ref
140600031124     c                   eval      Ptel   =  §AR5teld
140700030515      *
140800030515      * - Spedizione
140900030515      *   (numero: non se Rimesso al Mittente)
141000030515if  1c                   if        DB0ris <> 'M'
141100080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
141200080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
141300080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
141400030515e   1c                   endif
141500030515      * . . Data spedizione
141600031124     c     *iso          movel     Wdsp          Data_EUR
141700080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
141800030515      *
141900030515      * - Colli: numero, peso, volume, flag tipo-volume
142000080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
142100080911      /free
142200080911           if  NOT *in71;
142300080911      /end-free
142400080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
142500080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
142600030515if  1c                   if            DB0fvf <> *blanks
142700030515     c                             and DB0ris <> 'M'
142800081021     c                   movel     DB0fvf        Pfvf
142900030515e   1c                   endif
143000080915      /free
143100080915           endif;
143200080915      /end-free
143300030515      *
143400030515      * - Natura merce
143500030515     c                   eval      Pnat   =  DB0nas
143600030515      *
143700030515      * - Servizio
143800100517      /free
143900100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
144000110117           exsr  sr_CheckORM_PreavvMail;
144100100517         endif;
144200100517      /end-free
144300100517      * . . Descrizione porto
144400130925      *     (NON se presente ORM con preavviso mail)
144500100517     c                   if        Not  $PreavvORM
144600030515     c                   eval      Pds1   =  DB0dcb
144700100517     c                   endif
144800030515      * . . Descrizione tipo servizio
144900030515if  1c                   if        DB0fst =  'S'
145000030515     c                   eval      Pds2   =  DB0dts
145100030515e   1c                   endif
145200030515      * . . Reso / Franco frontiera
145300030515sel 1c                   select
145400030515w   1c                   when      DB0fbr =  'S'
145500030515     c                   clear                   w008a
145600030515     c                   eval      w008a  =  '   RESO '
145700030515     c                   move      w008a         Pds2
145800030515w   1c                   when      DB0fbr =  'F'
145900030515     c                   clear                   w015a
146000030515     c                   eval      w015a  =  '   FR.FRONTIERA'
146100030515     c                   move      w015a         Pds2
146200030515e   1c                   endsl
146300030515      *
146400080430      * - Zona consegna
146500080430      *   (campo alfanumerico)
146600080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
146700030515      *
146800030515      * - Flag "*" (gestito a parte)
146900040223      *
147000040223      * - Flag "V" = colli di valore
147100040223if  1c                   if        §AR5bva   <> *blank
147200040223     c                   eval      Pbva   =  'V'
147300040223e   1c                   endif
147400030515      *
147500030515      * - Fermo deposito
147600030515if  1c                   if        DB0ffd <> *blanks
147700080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
147800030515e   1c                   endif
147900030515      *
148000070308      * - Segnacolli: filiale, serie, dal / al
148100030515if  1c                   if        DB0fls <> *zeros
148200080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
148300030515e   1c                   endif
148400040607      * - - se bolla con mamma considero la serie della mamma
148500070515if  1c                   if        $Mamma =  *on
148600080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
148700031015x   1c                   else
148800080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
148900031015e   1c                   endif
149000030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
149100030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
149200030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
149300030515      *   Se collo singolo .......:   "     SCD
149400031015sel 1c                   select
149500030515      * - - non sequenziali
149600030515w   1c                   when      DB0fns =  'S'
149700131008if  2c                   if        DB0ncl <= 2
149800050511     c                   exsr      ListaScNSd
149900131008x   2c                   else
150000081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
150100131008e   2c                   endif
150200030515      * - - singolo o sequenziali
150300030515      *     (non se rimesso al mittente)
150400030515w   1c                   when      DB0ris <> 'M'
150500080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
150600030515if  2c                   if            DB0nca <> *zeros
150700030515     c                             and DB0nca <> DB0ncd
150800080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
150900080430     c                                    +  %editc(DB0nca : 'Z')
151000030515e   2c                   endif
151100030515e   1c                   endsl
151200030515      *
151300131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
151400030515     c                   exsr      SchPARTIC
151500131016e   1 *
151600030716if  1c                   if           $NP(1) <> *blanks
151700030716     c                             or $NP(2) <> *blanks
151800030716     c                             or $NP(3) <> *blanks
151900030716     c                             or $NP(4) <> *blanks
152000030716     c                             or $NP(5) <> *blanks
152100030716     c                             or $NP(6) <> *blanks
152200030710     c                   eval      Pnp1   =  $NP(1)
152300030710     c                   eval      Pnp2   =  $NP(2)
152400030710     c                   eval      Pnp3   =  $NP(3)
152500030710     c                   eval      Pnp4   =  $NP(4)
152600030710     c                   eval      Pnp5   =  $NP(5)
152700030710     c                   eval      Pnp6   =  $NP(6)
152800030716e   1c                   endif
152900030515      *
153000080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
153100080401      *   (servizio consegna garantita x cliente Wuerth)
153200170413if  1c*//                if        DB0gva =  'P '
153300170413     c*//                movel     'PRIVATO'     Ppad
153400170413e   1c*//                endif
153500041012if  1c                   if        DB0gva =  'U '
153600041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
153700041012e   1c                   endif
153800030515if  1c                   if           DB0tc1 =  'A'
153900030515     c                             or DB0tc2 =  'A'
154000030515     c                   move      'APPUNTAMENTO'Ppad
154100030515e   1c                   endif
154200080401if  1c                   if             DB0nrs = 40
154300090518     c                             and  DB0tsp = 'E'
154400080401     c                             and (DB0lnp = 043
154500080401     c                              or  DB0lnp = 006
154600080401     c                              or  DB0lnp = 063)
154700090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
154800090304     c                                     WÜRTH'
154900080401e   1c                   endif
155000030515      *
155100030515      * - Data e ora consegna richiesta
155200030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
155300091116      *   (NO "Prima del" per il cliente TTL)
155400091116if  0c                   if        DB0ccm <> c_CliTTL  or
155500091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
155600030515if  1c                   if        DB0dcr <> *zeros
155700030515     c                   eval      Past   =  '*'
155800030515     c     *iso          movel     DB0dcr        Data_EUR
155900030515     c     *eur          movel     Data_EUR      Pdcr
156000030515e   1c                   endif
156100030515if  1c                   if        DB0hcr <> *zeros
156200030515     c                   eval      Past   =  '*'
156300080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
156400030515e   1c                   endif
156500091116e   0c                   endif
156600030515      *
156700030515      * - Consegne particolari
156800030515     c                   eval      Pcp1   =  DB0dc1
156900030515     c                   eval      Pcp2   =  DB0dc2
157000030515      *
157100030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
157200030515      *   (importi "arrotondati" a 2 decimali in stampa)
157300030515if  1c                   if        DB0ift <> *zeros
157400080515     c                   eval      Pkim   = 'Imponibile'
157500080430     c                   eval(h)   w0112  =  DB0imv
157600080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
157700030515sel 2c                   select
157800030515w   2c                   when      DB0iva <> *zeros
157900080515     c                   eval      Pkie   = 'I.v.a.    '
158000030624     c                   eval(h)   w0112  =  DB0iva
158100080430     c                   eval      Piva   =  %editc(w0112 : 'K')
158200030515w   2c                   when         DB0cei <> *blanks
158300030515     c                             or DB0dce <> *blanks
158400130213     c                   eval      Pkie   = 'Esenzione '
158500130213     c                   eval      Pese   = '           ' +
158600130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
158700030515e   2c                   endsl
158800080515     c                   eval      Pktf   = 'Tot. Fatt.'
158900080430     c                   eval(h)   w0112  =  DB0ift
159000080430     c                   eval      Pift   =  %editc(w0112 : 'K')
159100030515if  2c                   if        DB0div <> §GEDcn
159200030515     c                   eval      Pdiv   =  DB0div
159300030515e   2c                   endif
159400030515e   1c                   endif
159500030515      *
159600030515      * - Contrassegno e relativa divisa
159700030515if  1c                   IF        DB0cas <> *zeros
159800030625      *
159900080430     c                   eval(h)   w0112  =  DB0cas
160000080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
160100030515if  2c                   if        DB0vca <> §GEDcn
160200030515     c                   eval      Pdca   =  DB0vca
160300030515e   2c                   endif
160400030515     c                   exsr      CtrCAs
160500030624      * - - Nuovo controllo abilitazione c/assegno
160600030624     c                   reset                   TRUL21DS
160700030624     c                   eval      I21cbo =  DB0cbo
160800030624     c                   eval      I21tsp =  DB0tsp
160900030624     c                   eval      I21lnp =  DB0lnp
161000030624     c                   eval      I21nzm =  DB0nzm
161100030624     c                   eval      I21lna =  DB0lna
161200030624     c                   eval      I21nzd =  DB0nzd
161300130826     c                   if        DB0ccm >  *zero
161400130826     c                   eval      I21ksc =  DB0ccm
161500130826     c                   else
161600130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
161700130826     c                   endif
161800030624     c                   eval      I21tic =  DB0tic
161900030624if  2c                   if        DB0vca <> §GEDcn
162000030624     c                   eval      I21imp =  Wcas
162100030624     c                   eval      I21div =  §GEDcn
162200030624x   2c                   else
162300030624     c                   eval      I21imp =  DB0cas
162400030624     c                   eval      I21div =  DB0vca
162500030624e   2c                   endif
162600080430     c                   eval      I21ute =  SDSusr
162700040607     c                   eval      I21pgm =  SDSpgm
162800030624     c                   call      'TRUL21R'
162900030624     c                   parm                    TRUL21ds
163000030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
163100030624     c                   exsr      Tab_1A
163200030624     c                   movel     §1Ades        DB0dti
163300030624if  2c                   if            §1Adeo <> *blanks
163400030624     c                             and O21fca <> *blanks
163500030624     c                   movel     §1Adeo        DB0dti
163600030624e   2c                   endif
163700030624      * - - Imposto i campi di stampa
163800080430     c                   eval      Pcti   =  DB0tic
163900080430     c                   eval      Ptic   =  DB0dti
164000030625      *
164100030515e   1c                   ENDIF
164200030515      *
164300030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
164400030515if  1c                   IF        DB0ift >  *zeros
164500030625      *
164600030515if  2c                   if            DB0vca <> §GEDcn
164700030515     c                             and DB0vca <> *blanks
164800030515     c                   eval      Pdie   =  DB0div
164900030515e   2c                   endif
165000030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
165100030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
165200030515      *   (importi "arrotondati" a 2 decimali in stampa)
165300030515sel 2c                   Select
165400110204      *   Non ho C/Assegno
165500101213w   2c                   When      DB0cas  = *zeros
165600030624     c                   eval(h)   w0112  =  DB0ift
165700030624     c                   eval      Pies   =  %editc(w0112 : 'K')
165800030625     c                   clear                   Pie1
165900030625     c                   clear                   Pdi1
166000030515      *   C/Assegno sommabile
166100030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
166200030515sel 3c                   select
166300030515      *   - c/assegno in EURO
166400030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
166500030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
166600030624     c                   eval      Pies   =  %editc(w0112 : 'K')
166700030624     c                   clear                   Pie1
166800030624     c                   clear                   Pdi1
166900030515      *   - c/assegno in DIVISA
167000030515w   3c                   when          DB0div =  §GEDcn
167100030515     c                             and DB0vca <> §GEDcn
167200030624     c                   eval(h)   w0112  =  DB0ift + Wcas
167300030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167400030624     c                   clear                   Pie1
167500030624     c                   clear                   Pdi1
167600030515e   3c                   endsl
167700100218      *   C/Assegno non sommabile
167800030515x   2c                   Other
167900030624     c                   eval(h)   w0112  =  DB0cas
168000030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
168100100218     c*//                eval      Pdi1   =  DB0vca
168200030624     c                   eval(h)   w0112  =  DB0ift
168300030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168400100218     c*//                eval      Pdie   =  DB0div
168500030515e   2c                   Endsl
168600030709      *
168700030709e   1c                   ENDIF
168800030515      *
168900030515      * - Numero e data fattura
169000030515if  1c                   if        DB0nft <> *zeros
169100080430     c                   eval      Pkft   =  'Fatt.'
169200080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
169300130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
169400130205     c                                      ')'
169500030515if  2c                   if        DB0dft >  *zeros
169600080430     c                   eval      Pkdf   =  ' del '
169700030515     c     *iso          movel     DB0dft        Data_EUR
169800130213     c     *dmy          movel     Data_EUR      Pdft
169900030515e   2c                   endif
170000030515e   1c                   endif
170100030515      *
170200030515      * Memorizzo il codice tariffa come numero di copie da stampare
170300030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
170400030515if  1c                   if        DB0ris = 'M'  and  *in70
170500030515     c                   eval      Wctr   =  DB0ctr
170600030515e   1c                   endif
170700030515      *
170800030515     c                   ENDSR
170900030515      *
171000030515      *---------------------------------------------------------------*
171100050217      *?Inschieramento delle Note e delle Particolarità              ?*
171200030515      *---------------------------------------------------------------*
171300030515     c     SchPARTIC     BEGSR
171400030515      *
171500130924     c***?(già fatto:)?  reset                   $NoDesPartic
171600130924     c***?(già fatto:)?  clear                   $NP
171700130924     c***?(già fatto:)?  clear                   W_CoOr
171800030515     c                   clear                   XX
171900030710      *
172000030710      * Imposto eventuali note per prime
172100030710if  1c                   if        DB0nt1 <> *blanks
172200030710     c                   add       1             xx
172300030710     c                   movel     DB0nt1        $NP(xx)
172400030710e   1c                   endif
172500030710if  1c                   if        DB0nt2 <> *blanks
172600030710     c                   add       1             xx
172700030710     c                   movel     DB0nt2        $NP(xx)
172800030710e   1c                   endif
172900030515      *
173000030515      * Le particolarità possono essere lunghe fino a 45 char.
173100030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
173200030515      *   bastare, il resto lo si mette nella riga successiva.
173300030710      *   Se, invece, non risultano decodificate, si mettono solo i
173400030710      *   codici sulla stessa riga.
173500030710      *   Se uno solo risulta decodificato, esso va su una riga a
173600030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
173700030710      *                                    *on  = ok riga procedente)
173800030515      *
173900030515      * . . Particolarità Giacenza
174000030515if  1c                   IF        DB0gga <> *blanks
174100030515     c                   movel     'Q'           WTpPar
174200030515     c                   movel     DB0gga        WCdPar
174300030515     c                   exsr      TABxPAR
174400030710if  2c                   if            TBLuni       = *blanks
174500030710     c                             and $NoDesPartic = *on
174600030710     c                   eval      $NP(xx) = %trim($NP(xx))
174700030710     c                                     + '   ' + WCdPar
174800030710x   2c                   else
174900030710if  3c                   if        TBLuni       = *blanks
175000030710     c                   eval      $NoDesPartic = *on
175100030710x   3c                   else
175200030710     c                   eval      $NoDesPartic = *off
175300030710e   3c                   endif
175400030515     c                   add       1             xx
175500100517     c                   movel     w053a         $NP(xx)
175600100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
175700030515     c                              <> *blanks
175800030515     c                   add       1             xx
175900030515     c                   eval      $NP(xx) = '   ' +
176000100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
176100030710e   3c                   endif
176200030515e   2c                   endif
176300030515e   1c                   ENDIF
176400030515      *
176500030515      * . . Particolarità Consegna
176600030515if  1c                   IF        DB0gma <> *blanks
176700030515     c                   movel     'R'           WTpPar
176800030515     c                   movel     DB0gma        WCdPar
176900030515     c                   exsr      TABxPAR
177000030710if  2c                   if            TBLuni       = *blanks
177100030710     c                             and $NoDesPartic = *on
177200030710     c                   eval      $NP(xx) = %trim($NP(xx))
177300030710     c                                     + '   ' + WCdPar
177400030710x   2c                   else
177500030710if  3c                   if        TBLuni       = *blanks
177600030710     c                   eval      $NoDesPartic = *on
177700030710x   3c                   else
177800030710     c                   eval      $NoDesPartic = *off
177900030710e   3c                   endif
178000030515     c                   add       1             xx
178100100517     c                   movel     w053a         $NP(xx)
178200100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
178300030515     c                              <> *blanks
178400030515     c                   add       1             xx
178500030515     c                   eval      $NP(xx) = '   ' +
178600100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
178700030710e   3c                   endif
178800030515e   2c                   endif
178900030515e   1c                   ENDIF
179000030515      *
179100030515      * . . Particolarità Varie
179200030717      *     - questa descrizione va messa nell'apposito campo PPAD
179300041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
179400030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
179500030717      *       apposito box
179600030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
179700030717      *       comparirà un apposito box, un'apposita nota e l'indica-
179800030717      *       zione nella copia per il destinatario.
179900030717      *     ...Potrebbero anche non esserci più elementi di schiera
180000030717      *     disponibili [xx >= %elem($NP)] !
180100030709if  1c                   IF            DB0gva <> *blanks
180200030717     c                             and DB0gva <> 'B '
180300030717     c                             and DB0gva <> 'O '
180400030709     c                             and DB0gva <> 'P '
180500041015     c                             and DB0gva <> 'U '
180600030515     c                   movel     'S'           WTpPar
180700030515     c                   movel     DB0gva        WCdPar
180800030515     c                   exsr      TABxPAR
180900030710if  2c                   if            TBLuni       = *blanks
181000030710     c                             and $NoDesPartic = *on
181100030710     c                   eval      $NP(xx) = %trim($NP(xx))
181200030710     c                                     + '   ' + WCdPar
181300030710x   2c                   else
181400030710if  3c                   if        TBLuni       = *blanks
181500030710     c                   eval      $NoDesPartic = *on
181600030710x   3c                   else
181700030710     c                   eval      $NoDesPartic = *off
181800030710e   3c                   endif
181900030717if  3c                   if            xx < %elem($NP)
182000030709     c                   add       1             xx
182100100517     c                   movel     w053a         $NP(xx)
182200100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
182300030709     c                              <> *blanks
182400030717     c                             and xx < %elem($NP)
182500030709     c                   add       1             xx
182600030709     c                   eval      $NP(xx) = '   ' +
182700100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
182800030717e   4c                   endif
182900030717e   3c                   endif
183000030709e   2c                   endif
183100030515e   1c                   ENDIF
183200030717      *
183300030717      * Imposto il numero originale di colli componenti la spedizione
183400030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
183500030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
183600030717     c                   exsr      RepAR5bnb
183700030717if  2c                   if        W_CoOr  <> *blanks
183800030717     c                             and xx  <  %elem($NP)
183900030717     c                   add       1             xx
184000030717     c                   eval      $NP(xx) =  W_CoOr
184100030717e   2c                   endif
184200030717e   1c                   endif
184300030515      *
184400030515     c                   ENDSR
184500070319      *
184600070319      *---------------------------------------------------------------*
184700070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
184800070319      *---------------------------------------------------------------*
184900070319     c     SchConsParz   BEGSR
185000070319      *
185100130924     c***?(già fatto:)?  reset                   $CP
185200130924     c***?(già fatto:)?  reset                   $CPdcm
185300070319     c                   clear                   XX
185400070319      *
185500070319if  2c                   if        NOT %open(FNART01L)
185600101213     c                   open(e)   FNART01L
185700101213      /free
185800101213             if  %error;
185900101213               exsr  sr_ErrOpenFile;
186000101213             endif;
186100101213      /end-free
186200070319e   2c                   endif
186300070319      *
186400070514if  2c                   if        $Mamma = *off
186500070319     c     KLDV          setll     FNART000
186600070319     c     KLDV          reade     FNART000
186700070514x   2c                   else
186800070514     c     KLBL          setll     FNART000
186900070514     c     KLBL          reade     FNART000
187000070514e   2c                   endif
187100070319      *
187200070319do  1c                   DOW       NOT  %eof(FNART01L)
187300070319      *
187400070319if  2c                   If             ARTatb = *blanks
187500070319     c                             and  ARTdcm > *zeros
187600070319      *
187700070319     c                   eval      *in31  = *off
187800070319     c                   eval      xx     = 1
187900070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
188000070319if  3c                   if        NOT  *in31
188100070319     c                   eval      xx     = 1
188200070319     c     *zeros        lookup    $CPdcm(xx)                             31
188300070319if  4c                   if        *in31
188400140304     c*//                eval      $CP(xx)     = *zeros
188500140304     c                   clear                   Elem_CP
188600140304     c                   movel     Elem_CP       $CP(xx)
188700070319e   4c                   endif
188800070319e   3c                   endif
188900070319if  3c                   if        *in31
189000070319     c                   eval      $CPdcm(xx)  = ARTdcm
189100070319     c                   movel     $CP(xx)       Elem_CP
189200070319     c                   eval      WCPdcm      = ARTdcm
189300070319     c                   eval      WCPncl      = WCPncl + 1
189400070319     c                   movel     Elem_CP       $CP(xx)
189500070319e   3c                   endif
189600070319      *
189700070319e   2c                   EndIf
189800070319      *
189900070514if  2c                   if        $Mamma = *off
190000070319     c     KLDV          reade     FNART000
190100070514x   2c                   else
190200070514     c     KLBL          reade     FNART000
190300070514e   2c                   endif
190400070319      *
190500070319e   1c                   ENDDO
190600070319      *
190700140514if  0c                   If        xx     > *zero
190800140513      * Reperimento nominativo firmatario   &
190900140514     c                   exsr      sr_Firmatario
191000070319      * Ordinamento schiera per data
191100070319if  1c                   if        xx     > 1
191200070319     c                   sorta                   $CP
191300070319e   1c                   endif
191400140514e   0c                   EndIf
191500070319      *
191600070319     c                   ENDSR
191700140303      *
191800140303      /free
191900140303
192000140303       //--------------------------------------------------------------
192100140303       //?Reperimento nome firmatario                                  ?
192200140303       //--------------------------------------------------------------
192300140303       BEGSR  sr_Firmatario;
192400140303
192500140303if  1    if  NOT  %open(FNEVB22L);
192600140303           open  FNEVB22L;
192700140303e   1    endif;
192800140303
192900140303         clear  keyFNEVB22;
193000140303         k_EVB2aas = DB0aas;
193100140303         k_EVB2lnp = DB0lnp;
193200140303         k_EVB2nrs = DB0nrs;
193300140303         k_EVB2nsp = DB0nsp;
193400140303         k_EVB2cev = 'P  ';
193500140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
193600140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
193700140303         DoW  Not  %eof(FNEVB22L);
193800140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
193900140304             yy = %lookup( EVB2dev : $CPdcm );
194000140304             if  yy > *zero;
194100140304               Elem_CP = $CP(yy);
194200140303               if  WCPocm  <= EVB2hev;
194300140303                 WCPocm  = EVB2hev;
194400140303                 WCPfir  = EVB2dati;
194500140304                 $CP(yy) = Elem_CP;
194600140303               endif;
194700140303             endif;
194800140303           endif;
194900140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
195000140303         EndDo;
195100140303
195200140303       ENDSR;
195300140303
195400140303      /end-free
195500031124      *
195600031124      *---------------------------------------------------------------*
195700050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
195800031124      *---------------------------------------------------------------*
195900031124     c     RepAR5gen     BEGSR
196000031124      *
196100031124     c                   clear                   dAR5gen
196200031124      *
196300031124     c                   eval      AR5trd = 'GEN'
196400031127     c     K05AR501      chain     FIAR5000
196500031124      *
196600101213if  1c                   if            %found(FIAR501L)
196700040301     c                             and AR5atb =  *blanks
196800031124     c                   movel     AR5uni        dAR5gen
196900031124e   1c                   endif
197000031124      *
197100031124     c                   ENDSR
197200030717      *
197300030717      *---------------------------------------------------------------*
197400050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
197500030717      *---------------------------------------------------------------*
197600030717     c     RepAR5bnb     BEGSR
197700030717      *
197800030717     c                   clear                   dAR5bnb
197900130924     c***?(già fatto:)?  clear                   W_CoOr
198000030717      *
198100030717     c                   eval      AR5trd = 'BNB'
198200030717     c     K05AR501      chain     FIAR5000
198300030717      *
198400101213if  1c                   if            %found(FIAR501L)
198500040301     c                             and AR5atb =  *blanks
198600030717     c                   movel     AR5uni        dAR5bnb
198700030717if  2c                   if        §AR5bcor <> *zeros
198800030717     c                   eval      W_CoOr   =  C_CoOr
198900030717     c                                      +  %editc(§AR5bcor : 'K')
199000030717e   2c                   endif
199100030717e   1c                   endif
199200030717      *
199300030717     c                   ENDSR
199400030515      *
199500030515      *---------------------------------------------------------------*
199600051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
199700030515      *---------------------------------------------------------------*
199800110208     c     sr_DecodLDV   BEGSR
199900030515      *
200000030515      * Richiesta firma per ricevuta della merce senza riserve
200100030515      * (e NON  quella  per quietanza) => *in35 + not *in36
200200030515     c                   movea     '10'          *in(35)
200300070307      *
200400070307      * Intestazione moduli
200500090619      *
200600090619      /free
200700121018
200800121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
200900121018         //  ?se Porto Assegnato con Fattura immediata?
201000121018         //  ?- NON se destinatario estero -?
201100121018             // -?con fattura?
201200121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
201300121018             // - -?P.A. (con fattura immediata)?
201400121018             //    ?(con destinatario italiano)?
201500121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
201600121018             // - -?con 2ª bolla?
201700121018             //    ?(con destinatario italiano)?
201800131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
201900121018           select;
202000131010             // ·?Esiste Partita Iva NON FITTIZIA?
202100131010             when  wB0piv <> *blank  and
202200121026                   %scan('$$' : wB0piv) = *zero;
202300121018               PpivD = 'Partita Iva: ' + wB0piv;
202400131010             // ·?Esiste Codice Fiscale?
202500131010             when  wB0cfi <> *blank;
202600121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
202700121018           endsl;
202800121018         ENDIF;
202900110204
203000110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
203100110204         //  ?(NON per Würth)?
203200110204         If  §VPOrcm <= wDate and
203300090619             DB0tsp   = 'E'   and
203400090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
203500090619                                   DB0lnp <> 006   and
203600090619                                   DB0lnp <> 063));
203700090619           clear  TNSD99ds;
203800101213           D98tbo = 'A';
203900090619           D98aas = DB0aas;
204000090619           D98lnp = DB0lnp;
204100090619           D98nrs = DB0nrs;
204200090619           D98nsp = DB0nsp;
204300090619           tnsd99r ( TNSD99ds );
204400120910           if  D98tcc = 12;
204500090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
204600090619           endif;
204700110204         EndIf;
204800110204
204900110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
205000110204         //  ?(stampa della sola copia per il vettore)?
205100110204         if  $No_x_Cons = *on;
205200110204           *in35 = *off;
205300110204           //*in36 = *off;
205400110204           // -?costante da stampare se LDV recapito c/assegni stampata?
205500110204           //  ?dal pgm "stampa DDT NO in arrivo"?
205600110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
205700110204         endif;
205800110204
205900110204         // -?Decodifica "Riserva Consegna" (se c'è)?
206000110204         clear  ds2A;
206100110204         If  ILVriserv <> *blank;
206200110204           TBLcod = '2A';
206300110204           TBLkey = ILVriserv;
206400110207           chain  K03TAB  TABEL;
206500110204           if  %found(TABEL00F);
206600110204             ds2A = TBLuni;
206700110204           endif;
206800110204         EndIf;
206900140514
207000140514         // -?Reperimento consegne parziali precedenti da FNART?
207100140514         If  $ConsParz;
207200140514           exsr  SchConsParz;
207300140514           if  xx < 1;
207400140514             $ConsParz = *off;
207500140514           endif;
207600140514         EndIF;
207700110204
207800110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
207900130716         exsr  sr_PerRicevutaDellaMerce;
208000110204
208100090619      /end-free
208200030515      *
208300130411      * Verifico se Packing-List e/o PDF
208400130411     c                   select
208500130411     c                   when      $§PDF  = *on
208600130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
208700130411     c                   when      $FIAR8 =  *on
208800130411     c                   eval      Ppkl   =  'PACKING LIST    '
208900130411     c                   endsl
209000030515      *
209100030515      * Flag "*" > x Consegne particolari
209200030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
209300030515     c                   eval      Past   =  '*'
209400030515     c                   endif
209500030515      *
209600030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
209700030515if  1c                   IF        DB0cas <> *zeros
209800030515      * - Flag "*" > x C/Assegno
209900030904if  2c                   if        O21fca <> *blanks
210000030904     c                   movel     '*'           Past
210100030515e   2c                   endif
210200030624      * - Imposto i campi di stampa
210300080430     c                   eval      Pcti   =  DB0tic
210400080430     c                   eval      Ptic   =  DB0dti
210500030515e   1c                   ENDIF
210600030515      *
210700040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
210800030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
210900030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
211000030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
211100030515     c                             or        DB0tb2     <> *blanks
211200030515     c                   eval      Pass   =  'A'
211300030515if  2c                   if        DB0nft =  *zeros
211400080430     c                   if        Pkie   =  *blanks
211500080430     c                   evalr     Pkie   =  'SEGUE'
211600080430     c                   eval      Piva   =  ' FATTURA'
211700080430     c                   else
211800080430     c                   eval      Piva   =  'SEGUE FATTURA'
211900080430     c                   endif
212000030515e   2c                   endif
212100030515e   1c                   endif
212200030515      *
212300030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
212400051129if  1c                   IF            DB0dcr >  *zeros
212500051129     c                             or  DB0hcr >  *zeros
212600051129     c                   clear                   FNLV80ds
212700051129     c                   eval      ILV80aas   =  DB0aas
212800051129     c                   eval      ILV80lnp   =  DB0lnp
212900051129     c                   eval      ILV80nrs   =  DB0nrs
213000051129     c                   eval      ILV80nsp   =  DB0nsp
213100051129     c                   eval      ILV80mgs   =  DB0mgs
213200051129     c                   eval      ILV80tc1   =  DB0tc1
213300051129     c                   eval      ILV80tc2   =  DB0tc2
213400051129     c                   eval      ILV80dcr   =  DB0dcr
213500051129     c                   eval      ILV80hcr   =  DB0hcr
213600051129     c                   eval      ILV80tcr   =  DB0tcr
213700051129     c                   call      'FNLV80R'
213800051129     c                   parm                    KPJBA
213900051129     c                   parm                    FNLV80ds
214000051129sel 2c                   Select
214100051129      * errore
214200051129w   2c                   When      OLV80err  <>  *blanks
214300051129      * consegna tassativa (per ora solo ai supermercati)
214400051129w   2c                   When      OLV80tat   =  'S'
214500081021     c                   movel     OLV80tcrd     Ptdr
214600091116      * NO "Prima del" per il cliente TTL
214700091116w   2c                   When      DB0ccm     = c_CliTTL   and
214800091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
214900051129      * prima/dopo il/delle
215000051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
215100110208      *  nella subr. "sr_TipoLDV")
215200081022x   2c                   Other
215300081021     c                   movel     OLV80tcrd     Ptdr
215400081021e   2c                   Endsl
215500051129e   1c                   ENDIF
215600030515      *
215700030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
215800060921      * (SE?non?è "Fermo Deposito")
215900030515      * 1° turno di chiusura
216000060921if  1c                   IF             DB0gc1 <> *blanks
216100060921     c                             and  DB0ffd  = *blanks
216200030515      * - Flag "*" > Turno di chiusura
216300030515     c                   eval      Past   =  '*'
216400030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
216500030515      * - Nessun giorno specificato - Solo 'M' o 'P'
216600130924     c***?già così:?     clear                   Pgc1
216700030515if  3c                   if        DB0gc1 =  ' M'
216800030515     c                   eval      Pgc1   =  %trim($Gio(8))
216900030515x   3c                   else
217000030515     c                   eval      Pgc1   =  %trim($Gio(9))
217100030515e   3c                   endif
217200030515x   2c                   Else
217300030515      * - Giorno specificato
217400030515     c                   movel     DB0gc1        w001a
217500030515     c                   eval      XX     =  1
217600030515     c                   eval      *in31  =  *off
217700030515     c     w001a         lookup    $GgS(xx)                               31
217800030515     c   31              movel     $Gio(xx)      Pgc1
217900030515      *   + 'M' o 'P'
218000030515sel 3c                   select
218100030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
218200030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
218300030515     c                                    +  %trim($Gio(8))
218400030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
218500030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
218600030515     c                                    +  %trim($Gio(9))
218700030515e   3c                   endsl
218800030515e   2c                   Endif
218900030515e   1c                   ENDIF
219000030515      * 2° turno di chiusura
219100060921if  1c                   IF             DB0gc2 <> *blanks
219200060921     c                             and  DB0ffd  = *blanks
219300030515      * - Flag "*" > Turno di chiusura
219400030515     c                   eval      Past   =  '*'
219500030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
219600030515      * - Nessun giorno specificato - Solo 'M' o 'P'
219700130924     c***?già così:?     clear                   Pgc2
219800030515if  3c                   if        DB0gc2 =  ' M'
219900080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
220000080515     c                                                = %trim($Gio(8))
220100030515x   3c                   else
220200080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
220300080515     c                                                = %trim($Gio(9))
220400030515e   3c                   endif
220500030515x   2c                   Else
220600030515      * - Giorno specificato
220700030515     c                   movel     DB0gc2        w001a
220800030515     c                   eval      XX     =  1
220900030515     c                   eval      *in31  =  *off
221000030515     c     w001a         lookup    $GgS(xx)                               31
221100080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
221200080515     c                                                = %trim($Gio(xx))
221300030515      *   + 'M' o 'P'
221400030515sel 3c                   select
221500030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
221600080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
221700030515     c                                    +  %trim($Gio(8))
221800030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
221900080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
222000030515     c                                    +  %trim($Gio(9))
222100030515e   3c                   endsl
222200030515e   2c                   Endif
222300030515e   1c                   ENDIF
222400080523      /free
222500080523       // ________________________________________
222600080523       // Impostazione nominativo del destinatario
222700080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
222800110214         exsr  RepAR4_1;
222900080523       // _______________________________________
223000080523       // Impostazione data ed ora della consegna
223100080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
223200110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
223300110228         //?riporta in tale campo la *date, NON quella impostata?
223400110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
223500110228         Data_EUR = *loval;
223600110209         clear  w0080;
223700110209         select;
223800110209           when  wB0dcm > *zero;
223900110209             Data_EUR = %date( wB0dcm : *iso );
224000110209           when  wB0dcp > *zero;
224100110209             Data_EUR = %date( wB0dcp : *iso );
224200110209         endsl;
224300110209         if  Data_EUR > *loval;
224400110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
224500110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
224600110209                     ( %subdt(Data_EUR : *years) ) );
224700110209         endif;
224800150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
224900081103           Pfedkcd  = %editc( w0080 : 'Y' );
225000120702           if  dILVlibero.§ILVhmp > *zero;
225100081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
225200120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
225300120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
225400081103           endif;
225500081103         else;
225600081103           Pkcd  = %editc( w0080 : 'Y' );
225700120702           if  dILVlibero.§ILVhmp > *zero;
225800081103             Pkcd = %trimr( Pkcd ) + '   '
225900120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
226000120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
226100081103           endif;
226200081103         endif;
226300080523      /end-free
226400071114      *
226500071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
226600030515      *
226700071114      * Se pgm richiamato x stampa LdV in arrivo:
226800030515if  1c                   IF        DB0fl1 =  'A'
226900071114      * - imposto l'"*" in base al parametro ricevuto.
227000030515if  2c                   if        DB0fl2 =  '*'
227100030515     c                   eval      Past   =  '*'
227200030515x   2c                   else
227300030515     c                   clear                   Past
227400030515e   2c                   endif
227500071114      * - idem per flag di bolla non abilitata
227600030515     c                   eval      Pdis   =  DB0fl5
227700030515e   1c                   endif
227800030515      *
227900030515     c                   ENDSR
228000080430      *
228100080430      /free
228200130716
228300130716       //--------------------------------------------------------------
228400130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
228500130716       //--------------------------------------------------------------
228600130716       BEGSR  sr_PerRicevutaDellaMerce;
228700130716
228800130716         Select;
228900130716
229000130716           // -?danon?stampare se?
229100130716           // ·?non utilizzare per la consegna  (NOT *in35)?
229200130716           When  NOT *in35;
229300130716             //clear  Pkrm;       ?(già fatto!)?
229400140108             //clear  Pkrm2;      ?(già fatto!)?
229500130716
229600130716           // -?costante per "DPD"   (come standard)?
229700130716           //         ?o per "FedEx" (come standard)?
229800130716           When  *in71  or  *in72;
229900130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
230000130716             // -?senza riserve?
230100130716             if  ILVriserv = *blank;
230200130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
230300130716             // -?con Riserva Consegna?
230400130716             else;
230500130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
230600130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
230700130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
230800130716               endif;
230900130716             endif;
231000130716
231100130716           // -?costante per cliente?
231200130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
231300140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
231400130716             exsr  sr_RepDdtCLN;
231500130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
231600130716                     DDT '
231700130716                  + %trimr( %editc (WddtCLN : 'Z') )
231800130716                  + ' DEL '
231900130716                  + %editc (WdspCLN : 'Y')
232000130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
232100130716             if  Wdsp < 20101001;
232200130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
232300130716             else;
232400130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
232500130716             endif;
232600130716
232700130716           // -?costante "standard" se "DDT sì"?
232800130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
232900130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
233000130716                     TRASPORTO';
233100130716             // -?senza riserve?
233200130716             if  ILVriserv = *blank;
233300130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
233400130716             // -?con Riserva Consegna?
233500130716             else;
233600130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
233700130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
233800130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
233900130716               endif;
234000130716             endif;
234100130716
234200130716           // -?costante "standard" se "DDT no" + Packing-List?
234300161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
234400130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
234500130716                     TRASPORTO';
234600130716             // -?senza riserve?
234700130716             if  ILVriserv = *blank;
234800130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
234900130716             // -?con Riserva Consegna?
235000130716             else;
235100130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
235200130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
235300130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
235400130716               endif;
235500130716             endif;
235600130716
235700130716           // -?costante "standard" altrimenti?
235800130716           Other;
235900130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
236000130716             // -?senza riserve?
236100130716             if  ILVriserv = *blank;
236200130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
236300130716             // -?con Riserva Consegna?
236400130716             else;
236500130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
236600130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
236700130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
236800130716               endif;
236900130716             endif;
237000130716
237100130716         EndSl;
237200130716
237300130716         // -?costante aggiuntiva per cliente?
237400130716         //  ?"S G Soluzioni Logistiche"?
237500130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
237600130716
237700130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
237800130716             Pkrm2 = 'INCLUDE CONTRATTO - '
237900130716                   + %trim(DB0nas);
238000130716           endif;
238100130716           if  DB0rmo <> *blank;
238200130716             if  Pkrm2 <> *blank;
238300130716               Pkrm2 = %trimr(Pkrm2)
238400130716                     + ' - MATR TV: '
238500130716                     + %trim(DB0rmo);
238600130716             else;
238700130716               Pkrm2 = 'MATR TV: '
238800130716                     + %trim(DB0rmo);
238900130716             endif;
239000130716           endif;
239100130716
239200130716         EndIf;
239300140108
239400140108         // -?Consegne Parziali Precedenti?
239500140108         If  $ConsParz = *on  and  wB0dcm = *zero;
239600140108
239700140108           Select;
239800140110             When  Pkrm2 = *blank;
239900140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
240000140108             When  %subst( Pkrm : 111 ) = *blank;
240100140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
240200140108             When  %subst( Pkrm2 : 111 ) = *blank;
240300140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
240400140108           EndSl;
240500140108
240600140108         EndIf;
240700130716
240800130716       ENDSR;
240900090619
241000090619       //--------------------------------------------------------------
241100130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
241200130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
241300090619       //--------------------------------------------------------------
241400090619       BEGSR  sr_RepDdtCLN;
241500090619
241600130716         // -?Verifica se la spedizione ha mamma?
241700090619         if  not %open(FNLBL01L);
241800101213           open(e)  FNLBL01L;
241900101213           if  %error;
242000101213             exsr  sr_ErrOpenFile;
242100101213           endif;
242200090619         endif;
242300090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
242400090619
242500090619         Select;
242600090619
242700130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
242800090619           When  %found(FNLBL01L);
242900090619             if  not %open(FNBLP01L);
243000101213               open(e)  FNBLP01L;
243100101213               if  %error;
243200101213                 exsr  sr_ErrOpenFile;
243300101213               endif;
243400090619             endif;
243500090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
243600090619             if  %found(FNBLP01L);
243700090619               exsr  sr_DatiCLN_blp;
243800090619             endif;
243900090619
244000130716           // -?In Arrivo: reperimento dati DDT da FNARB?
244100090619           Other;
244200090619             exsr  sr_DatiCLN_arb;
244300090619
244400090619         EndSl;
244500090619
244600090619       ENDSR;
244700090619
244800090619       //--------------------------------------------------------------
244900090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
245000090619       //--------------------------------------------------------------
245100090619       BEGSR  sr_DatiCLN_blp;
245200090619
245300130716         // -?Numero DDT?
245400090619         select;
245500090619           when  BLPrmn <> *zero;
245600100906             if  BLPrmn  > -99999999   and
245700100906                 BLPrmn <   99999999;
245800100906               WddtCLN = BLPrmn;
245900100906             endif;
246000090619           when  BLPncd <> *zero;
246100090619             WddtCLN = BLPncd;
246200090619         endsl;
246300090619
246400130716         // -?Data DDT in formato gg/mm/aaaa?
246500090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
246600100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
246700100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
246800100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
246900090619
247000090619       ENDSR;
247100090619
247200090619       //--------------------------------------------------------------
247300090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
247400090619       //--------------------------------------------------------------
247500090619       BEGSR  sr_DatiCLN_arb;
247600090619
247700130716         // -?Numero DDT?
247800090619         select;
247900090619           when  ARBrmn <> *zero;
248000100906             if  ARBrmn  > -99999999   and
248100100906                 ARBrmn <   99999999;
248200100906               WddtCLN = ARBrmn;
248300100906             endif;
248400090619           when  ARBncd <> *zero;
248500090619             WddtCLN = ARBncd;
248600090619         endsl;
248700090619
248800130716         // -?Data DDT in formato gg/mm/aaaa?
248900090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
249000090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
249100090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
249200110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
249300090619
249400090619       ENDSR;
249500100517
249600100517       //--------------------------------------------------------------
249700100517       //?Verifica se presente ORM con preavviso mail                  ?
249800100517       //--------------------------------------------------------------
249900110117       BEGSR  sr_CheckORM_PreavvMail;
250000100517
250100130924         //clear  dOrm01;             ?(già fatto)?
250200100517
250300130925         // -?Reperimento num. ORM?
250400130924         exsr  RepAR4_M;
250500100517
250600130925         // -?SE ORM inesistente: inutile proseguire?
250700130925         if  §B4poe = *zero   or   §B4nor = *zero;
250800100517           leavesr;
250900100517         endif;
251000100517
251100130716         // -?Verifica presenza O.R.M.?
251200100517         if  Not  %open(FNORM01L);
251300101213           open(e)  FNORM01L;
251400101213           if  %error;
251500101213             exsr  sr_ErrOpenFile;
251600101213           endif;
251700100517         endif;
251800100517
251900100517         ORMpoe = §B4poe;
252000100517         ORMnsr = §B4nsr;
252100100517         ORMnor = §B4nor;
252200100517         ORMnrv = §B4nrv;
252300100517
252400100517         chain  K04ORM01  FNORM000;
252500100517
252600130716         // -?ORM non reperito: inutile proseguire?
252700100517         if  Not  %found(FNORM01L);
252800100517           leavesr;
252900100517         endif;
253000100517
253100130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
253200100517         dOrm01 = ORMflo;
253300100517         $PreavvORM = (§ORMpre = 'M');
253400100517
253500100517       ENDSR;
253600090619
253700080430       //--------------------------------------------------------------
253800140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
253900080430       //--------------------------------------------------------------
254000131016       BEGSR  sr_DatiOUT;
254100080430
254200101221         // -?Impostazione righe?
254300080430         clear LdV;
254400080430         clear wx;
254500080430
254600101221         // -?Area immagine LdV?
254700080801
254800110203         FOR  wx = 1  TO  c_Row;
254900080430
255000080430           %occur(LdV) = wx;
255100080430
255200080508           LdvPrg      = wx;
255300080430           select;
255400080430
255500101221             // ·? 1ª riga?
255600110203             when  wx = 1;
255700110203             LdvTxt = ds_01;
255800080430
255900101221             // ·? 2ª riga (vuota)?
256000110203             when  wx = 2;
256100080430             clear LdvTxt;
256200080430
256300101221             // ·? 3ª riga?
256400110203             when  wx = 3;
256500110203             LdvTxt = ds_03;
256600080430
256700101221             // ·? 4ª riga?
256800110203             when  wx = 4;
256900110203             LdvTxt = ds_04;
257000080430
257100101221             // ·? 5ª riga?
257200110203             when  wx = 5;
257300110203             LdvTxt = ds_05;
257400080430
257500101221             // ·? 6ª riga (vuota)?
257600110203             when  wx = 6;
257700110203             clear LdvTxt;
257800080430
257900101221             // ·? 7ª riga?
258000110203             when  wx = 7;
258100110203             LdvTxt = ds_07;
258200080430
258300101221             // ·? 8ª riga (vuota)?
258400110203             when  wx = 8;
258500080430             clear LdvTxt;
258600080430
258700101221             // ·? 9ª riga?
258800110203             when  wx = 9;
258900110203             LdvTxt = ds_09;
259000080430
259100101221             // ·?10ª riga?
259200110203             when  wx = 10;
259300110203             LdvTxt = ds_10;
259400080430
259500101221             // ·?11ª riga?
259600110203             when  wx = 11;
259700110203             LdvTxt = ds_11;
259800080430
259900101221             // ·?12ª riga?
260000110203             when  wx = 12;
260100110203             LdvTxt = ds_12;
260200080430
260300101221             // ·?13ª riga?
260400110203             when  wx = 13;
260500110203             LdvTxt = ds_13;
260600080430
260700101221             // ·?14ª riga?
260800110203             when  wx = 14;
260900110203             LdvTxt = ds_14;
261000080430
261100101221             // ·?15ª riga?
261200110203             when  wx = 15;
261300110203             LdvTxt = ds_15;
261400080430
261500101221             // ·?16ª riga?
261600110203             when  wx = 16;
261700110203             LdvTxt = ds_16;
261800080430
261900101221             // ·?17ª riga?
262000110203             when  wx = 17;
262100110203             LdvTxt = ds_17;
262200080430
262300101221             // ·?18ª riga?
262400110203             when  wx = 18;
262500110203             LdvTxt = ds_18;
262600080430
262700101221             // ·?19ª riga?
262800110203             when  wx = 19;
262900110203             LdvTxt = ds_19;
263000080430
263100101221             // ·?20ª riga?
263200110203             when  wx = 20;
263300110203             LdvTxt = ds_20;
263400080430
263500101221             // ·?21ª riga?
263600110203             when  wx = 21;
263700110203             LdvTxt = ds_21;
263800080430
263900101221             // ·?22ª riga?
264000110203             when  wx = 22;
264100110203             LdvTxt = ds_22;
264200080430
264300101221             // ·?23ª riga?
264400110203             when  wx = 23;
264500110203             LdvTxt = ds_23;
264600080430
264700101221             // ·?24ª riga?
264800110203             when  wx = 24;
264900110203             LdvTxt = ds_24;
265000080430
265100101221             // ·?25ª riga?
265200110203             when  wx = 25;
265300110203             LdvTxt = ds_25;
265400080430
265500101221             // ·?26ª riga (vuota)?
265600110203             when  wx = 26;
265700080806             clear LdvTxt;
265800080430
265900101221             // ·?27ª riga?
266000110203             when  wx = 27;
266100110203             LdvTxt = ds_27;
266200080430
266300101221             // ·?28ª riga?
266400110203             when  wx = 28;
266500110203             LdvTxt = ds_28;
266600081021
266700101221             // ·?29ª riga?
266800110203             when  wx = 29;
266900081021             LdvTxt = ds_29;
267000081021
267100101221             // ·?30ª riga?
267200110203             when  wx = 30;
267300081021             LdvTxt = ds_30;
267400081021
267500110215             // ·?31ª riga?
267600110203             when  wx = 31;
267700110215             LdvTxt = ds_31;
267800081021
267900101221             // ·?32ª riga?
268000110203             when  wx = 32;
268100081021             LdvTxt = ds_32;
268200081021
268300101221             // ·?33ª riga?
268400110203             when  wx = 33;
268500081021             LdvTxt = ds_33;
268600081021
268700101221             // ·?34ª riga?
268800110203             when  wx = 34;
268900081021             LdvTxt = ds_34;
269000081021
269100101221             // ·?35ª riga (vuota)?
269200110203             when  wx = 35;
269300081021             clear LdvTxt;
269400081021
269500101221             // ·?36ª riga (vuota)?
269600110203             when  wx = 36;
269700081021             clear LdvTxt;
269800080430
269900080430           endsl;
270000080430
270100080430         ENDFOR;
270200080430
270300110518         // -?Impostazione iniziale del modulo standard:?
270400131203         //// ·?Modulo standard con logo Bartolini?
270500131203         //TemplateName = 'ldvbase00 ';
270600131203         //// ·?Modulo standard con logo Bartolini-BRT?
270700131203         //TemplateName = 'ldvbase10 ';
270800120524         // ·?Modulo standard con logo BRT?
270900120524         TemplateName = 'ldvbase20 ';
271000080801
271100110203         Select;
271200081030
271300101221           // -?Area dati "DPD"?
271400101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
271500110203           When  §CMntw = 'DPD';
271600110203             exsr  sr_LdVbaseDPD;
271700081030
271800101221           // -?Area dati "FedEx"?
271900150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
272000110203             exsr  sr_LdVbaseFED;
272100081030
272200131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
272300131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
272400110203             exsr  sr_LdVbase01;
272500081030
272600110203         EndSl;
272700130411
272800130411
272900130411         // ? CHIODI PER TEST                               ?
273000130411         Select;
273100130411           // -?Nessun TEST?
273200130411           When  $Test = *blank;
273300130411           // -?Spedizione FedEx?
273400130411           When  $Test = 'FED';
273500130411             TemplateName = 'ldvbase23 ';
273600130411           // -?Spedizione DPD?
273700130411           When  $Test = 'DPD';
273800130411             TemplateName = 'ldvbase22 ';
273900130411           // -?Spedizione con Consegna Parziale Precedente?
274000130411           When  $Test = 'C_P';
274100130411             TemplateName = 'ldvbase21 ';
274200130411           // -?Spedizione Normale?
274300130411           When  $Test = 'NOR';
274400130411             TemplateName = 'ldvbase20 ';
274500130411         EndSl;
274600130411
274700080801
274800101221         // -?Vuoti finali?
274900130213         For  wx = wx  To  w_Row;
275000080801           %occur(LdV) = wx;
275100130213           if  LdVPrg = *zero;
275200130213             LdvPrg = wx;
275300130213             clear  LdvTxt;
275400130213           endif;
275500110203         EndFor;
275600130205
275700130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
275800130205         if  DB0ift <> *zero  and
275900130205             DB0iva =  *zero  and
276000130205            (DB0cei <> *blank  or  DB0dce <> *blank);
276100130213           w_Row += 1;
276200130213           wx = w_Row;
276300130205           %occur(LdV) = wx;
276400130205           LdvPrg = 140;
276500130205           LdvTxt = ds_140;
276600130205         endif;
276700130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
276800130205         if  DB0nft <> *zero;
276900130213           w_Row += 1;
277000130213           wx = w_Row;
277100130205           %occur(LdV) = wx;
277200130205           LdvPrg = 180;
277300130205           LdvTxt = ds_180;
277400130205         endif;
277500110201
277600110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
277700110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
277800110201         reset  TRUL28ds;
277900110201         I28cod = BarCodeDS;
278000110201         TRUL28R1 ( TRUL28ds );
278100110201
278200110201         // -?Impostazione campo "LastRow"?
278300110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
278400110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
278500120419                                + %subst( ILVprmDst : 4 );
278600080801
278700110203         // ______________________________
278800110203         //?Stampa di prova (SE richiesta)?
278900110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
279000110519         If  SDSprm > 11;
279100130716           exsr  sr_Print;
279200130716         EndIf;
279300080430
279400080430       ENDSR;
279500130716
279600130716       //--------------------------------------------------------------
279700130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
279800130716       //--------------------------------------------------------------
279900130716       BEGSR  sr_Print;
280000130716
280100130716         // -?Apertura file di stampa?
280200130716         if not %open(QSYSPRT);
280300130716           open  QSYSPRT;
280400130716         endif;
280500130716
280600130716         // -?Stampa "testata"?
280700130716         except PRTtxt;
280800130716
280900130716         // -?Stampa dettaglio?
281000130716         for wx = 1 to w_Row;
281100130716           %occur(LdV) = wx;
281200130716           except PRTrow;
281300130716         endfor;
281400130716
281500130716         // -?Stampa "ultima riga"?
281600130716         except PRTend;
281700130716
281800130716         // -?Chiusura file di stampa?
281900130716         close QSYSPRT;
282000130716
282100130716       ENDSR;
282200081030
282300081030       //--------------------------------------------------------------
282400130716       //?Preparazione dati x spedizione "DPD"                         ?
282500081030       //--------------------------------------------------------------
282600130716       BEGSR  sr_LdVbaseDPD;
282700081030
282800120524         // -?Impostazione del modulo DPD:?
282900131203         //// ·?Modulo DPD con logo Bartolini?
283000131203         //TemplateName = 'ldvbase02 ';
283100131203         //// ·?Modulo DPD con logo Bartolini/BRT?
283200131203         //TemplateName = 'ldvbase12 ';
283300120524         // ·?Modulo DPD con logo BRT?
283400120524         TemplateName = 'ldvbase22 ';
283500081030
283600101221         // -?Impostazione data?
283700081030         %occur(LdV) = 19;
283800081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
283900081030
284000101221         // -?Impostazione numero colli?
284100081030         %occur(LdV) = 20;
284200081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
284300081030
284400101221         // -?Impostazione numero parcel?
284500081030         %occur(LdV) = 21;
284600081030         %subst(LdVTxt : 94) = §B4Ipn;
284700081111
284800131015         // -?"Decodifica" del barcode?
284900081111         %occur(LdV) = 22;
285000081111         %subst(LdVTxt : 94) = *blank;
285100081111         if  §B4Ipn <> *blank;
285200101221           // -?controllo se vecchio o nuovo barcode?
285300081111           if  %subst(§B4Ipn : 14 : 1) = *blank;
285400081111             // - - vecchio
285500081111             %subst(LdVTxt : 94) = '01 ' + %subst(§B4Ipn : 1 : 3)
285600081111                                 + ' '   + %subst(§B4Ipn : 4 : 8);
285700081111           else;
285800081111             // - - nuovo
285900081111             %subst(LdVTxT : 94) = '01 ' + %subst(§B4Ipn : 1 :  4)
286000081111                                 + ' '   + %subst(§B4Ipn : 5 : 10);
286100081111           endif;
286200081111         endif;
286300081111
286400130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
286500081111         //%occur(LdV) = 23;
286600081111         //%subst(LdVTxt : 94) = *blank;
286700081111         //%occur(LdV) = 24;
286800081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
286900081111         //%occur(LdV) = 25;
287000081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
287100081111         //%occur(LdV) = 26;
287200081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
287300081111         //%occur(LdV) = 27;
287400081111         //%subst(LdVTxt : 94) = 'DPD.';
287500081030
287600081030       ENDSR;
287700081030
287800081030       //--------------------------------------------------------------
287900130716       //?Preparazione dati x spedizione "FedEx"                       ?
288000081030       //--------------------------------------------------------------
288100130716       BEGSR  sr_LdVbaseFED;
288200081030
288300120524         // -?Impostazione del modulo FedEx:?
288400131203         //// ·?Modulo FedEx con logo Bartolini?
288500131203         //TemplateName = 'ldvbase03 ';
288600131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
288700131203         //TemplateName = 'ldvbase13 ';
288800120524         // ·?Modulo FedEx con logo BRT?
288900120524         TemplateName = 'ldvbase23 ';
289000110517
289100081103         w_Row  = c_Row_B;
289200081030         Wtrc = 'C';
289300081030
289400101221         // -?Preparazione elenco segnacolli FedEx?
289500101213         if  NOT %open(FNART01L);
289600101213           open(e)  FNART01L;
289700101213           if  %error;
289800101213             exsr  sr_ErrOpenFile;
289900101213           endif;
290000101213         endif;
290100101213         if  NOT %open(FIARS01L);
290200101213           open(e)  FIARS01L;
290300101213           if  %error;
290400101213             exsr  sr_ErrOpenFile;
290500101213           endif;
290600101213         endif;
290700081030
290800101221         // -?Impostazione numeratore fino alla 39ª riga?
290900101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
291000081030
291100081030           %occur(LdV) = wx;
291200081030           LdvPrg      = wx;
291300081030
291400101213         ENDFOR;
291500081030
291600101221         // -?40ª riga & successive?
291700081030         clear  xx;
291800081030         clear  Wnsc;
291900101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
292000081030
292100081030           %occur(LdV) = wx;
292200081030           LdvPrg      = wx;
292300081030
292400081030           // - Caricamento AWB FedEx
292500081103           clear  wAWB;
292600101221           // - -?leggo segnacolli per bolle in arrivo?
292700081030           exsr  CarAWB_Arx;
292800081030           if  wAWB = *blank;
292900081030             leave;
293000081030           endif;
293100081030
293200081030           xx    += 1;
293300081103           Pptn   = wAWB;
293400081030           Psts   = %editc( xx : 'Z' );
293500081030           LdvTxt = ds_Fed;
293600081030
293700101221         ENDFOR;
293800081030
293900081030       ENDSR;
294000081030
294100081030       //--------------------------------------------------------------
294200130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
294300081030       //--------------------------------------------------------------
294400130716       BEGSR  sr_LdVbase01;
294500081030
294600120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
294700131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
294800131203         //TemplateName = 'ldvbase01 ';
294900131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
295000131203         //TemplateName = 'ldvbase11 ';
295100120524         // ·?Modulo Box aggiuntivo con logo BRT?
295200120524         TemplateName = 'ldvbase21 ';
295300110517
295400081103         w_Row  = c_Row_B;
295500081030
295600101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
295700131206         //  ?(NON LA SI FA PIÙ)?
295800131206         //exsr  sr_IntestBox;
295900131206
296000131206
296100131206         // -?Posizionamento sulla 36ª riga?
296200131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
296300131206         //  ? sulla 37ª riga)?
296400131206         wx = 36;
296500131206
296600131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
296700131206         IF  $1o_Passaggio;
296800131206
296900131206           test(de) EVBdev;
297000131206           if %error;
297100131206             clear w0080;
297200131206           else;
297300131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
297400131206             w0080    = %dec(Data_Eur);
297500131206           endif;
297600131206
297700131206           // -?37ª riga?
297800131206           wx += 1;
297900131206           %occur(LdV) = wx;
298000131206           LdvPrg      = wx;
298100131206           Ppma    = 'Primo passaggio del '
298200131206                   + %editc( w0080 : 'Y' )
298300131206                   + ' effettuato alle '
298400131206                   + %editw( EVBhev: '  :  ' );
298500131206           LdvTxt  = ds_37;
298600131206
298700131206           // -?38ª riga: VUOTA (separatrice)?
298800131206           wx += 1;
298900131206           %occur(LdV) = wx;
299000131206           LdvPrg      = wx;
299100131206           clear  LdvTxt;
299200131206
299300131206         ENDIF;
299400131206
299500081030
299600131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
299700131206         IF  $ConsParz = *on;
299800131206
299900131206           // -?Impostazione segnalazione in output?
300000131206           xx = 1;
300100131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
300200131206
300300131206             Elem_CP = $CP(xx);
300400131206             test(de) WCPdcm;
300500131206             if %error;
300600131206               clear w0080;
300700131206             else;
300800131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
300900131206               w0080    = %dec(Data_Eur);
301000131206             endif;
301100131206
301200131206             // -?37ª o 39ª riga & successive?
301300131206             wx += 1;
301400131206             %occur(LdV) = wx;
301500131206             LdvPrg      = wx;
301600131206             Ppma    = 'Consegnati colli '
301700131206                     + %editc(WCPncl:'Z')
301800131206                     + ' in data '
301900140304                     + %editc(w0080:'Y');
302000140304             if  WCPocm > *zero;
302100140304               Ppma    = %trimr( Ppma )
302200140304                       + ' alle ore '
302300140304                       + %editw( WCPocm : '0 :  ' );
302400140304             endif;
302500140304             if  WCPfir <> *blank;
302600140304               Ppma    = %trimr( Ppma )
302700140303                     + ' - Firmatario: '
302800140303                     + WCPfir;
302900140304             endif;
303000131206             LdvTxt  = ds_37;
303100131206
303200131206             xx += 1;
303300131206
303400131206           EndDo;
303500131206
303600131206         ENDIF;
303700081030
303800081030       ENDSR;
303900080801
304000080801       //--------------------------------------------------------------
304100130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
304200080801       //--------------------------------------------------------------
304300131206       //BEGSR  sr_IntestBox;
304400131206       //
304500131206       //  // 37ª riga
304600131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
304700131206       //  %occur(LdV) = wx;
304800131206       //  LdvPrg      = wx;
304900131206       //  Ppma    = '                             '
305000131206       //          + c_PMxA;
305100131206       //  LdvTxt  = ds_37;
305200131206       //
305300131206       //  // -?38ª riga?
305400131206       //  wx += 1;
305500131206       //  %occur(LdV) = wx;
305600131206       //  LdvPrg      = wx;
305700131206       //  Ppma    = *all'=';
305800131206       //  LdvTxt  = ds_37;
305900131206       //
306000131206       //ENDSR;
306100101221
306200080430      /end-free
306300081030      *
306400081030      *---------------------------------------------------------------*
306500081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
306600081030      *---------------------------------------------------------------*
306700081030     c     CarAWB_Arx    BEGSR
306800130215      *
306900130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
307000130215      *   comunque una bolla mamma
307100130215     c                   exsr      sr_Mamma_FedEx
307200081030      *
307300081030      * Posizionamento
307400130215if  2c                   if        $Mamma = *off   and
307500130215     c                             $Mamma_FedEx = *off
307600081030     c     KLDVnsc       setgt     FNART000
307700081030x   2c                   else
307800081030     c     KLBLnsc       setgt     FNART000
307900081030e   2c                   endif
308000081030      *
308100081030      * Ciclo di lettura
308200081030do  1c                   DO        *hival
308300081030      *
308400130215if  2c                   if        $Mamma = *off   and
308500130215     c                             $Mamma_FedEx = *off
308600081030     c     KLDV          reade     FNART000
308700081030x   2c                   else
308800081030     c     KLBL          reade     FNART000
308900081030e   2c                   endif
309000081030if  2c                   if        %eof(FNART01L)
309100081030     c                   leave
309200081030e   2c                   endif
309300081030      * - per ogni segnacollo cerco l'AWB
309400081030     c     K05ARS01A     chain     FIARS000
309500081030if  2c                   if            %found(FIARS01L)
309600081030     c                             and %subst(ARSnot:1:1) >= *zeros
309700081030     c                   eval      wAWB = %subst(ARSnot:1:12)
309800081030     c                   eval      wNSC = ARSnsc
309900081030     c                   leave
310000081030e   2c                   endif
310100081030      *
310200081030e   1c                   ENDDO
310300081030      *
310400081030     c                   ENDSR
310500130215      *
310600130215      /free
310700130215
310800130215       //--------------------------------------------------------------
310900130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
311000130215       //--------------------------------------------------------------
311100130215       BEGSR  sr_Mamma_FedEx;
311200130215
311300130924         //$Mamma_FedEx = *off;         ?(già così)?
311400130215
311500130924         If  Not $Mamma;
311600130215
311700130215           // -?Verifica se la spedizione ha comunque mamma?
311800130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
311900130215           if  not %open(FNLBL01L);
312000130215             open  FNLBL01L;
312100130215           endif;
312200130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
312300130215
312400130215           $Mamma_FedEx = %found(FNLBL01L);
312500130215
312600130215         EndIf;
312700130215
312800130215       ENDSR;
312900130215
313000130215      /end-free
313100131008      *
313200131008      *---------------------------------------------------------------*
313300131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
313400131008      *?  da FNART (arrivo)                                          ?*
313500131008      *---------------------------------------------------------------*
313600131008     c     ListaScNSd    BEGSR
313700131008      *
313800131008      * SE ci sono solo 2 segnacolli non sequenziali:
313900131008      *   staranno al posto del range - basta non indicare
314000131008      *   "NON SEQUENZIALI"
314100131008      *
314200131008if  1c                   if        not %open(FNART01L)
314300131008     c                   open(e)   FNART01L
314400131008      /free
314500131008           if  %error;
314600131008             exsr  sr_ErrOpenFile;
314700131008           endif;
314800131008      /end-free
314900131008e   1c                   endif
315000131008      *
315100131008      * memorizzo l'ultimo numero di segnacollo
315200131008if  1c                   if        $Mamma = *off
315300131008     c     KLDV          setgt     FNART000
315400131008     c     KLDV          readpe    FNART000
315500131008x   1c                   else
315600131008     c     KLBL          setgt     FNART000
315700131008     c     KLBL          readpe    FNART000
315800131008e   1c                   endif
315900131008if  1c                   if        NOT %eof(FNART01L)
316000131008     c                   eval      %subst(Pnsx : 8) = ' , '
316100131008     c                                              + %editc(ARTnsc : 'Z')
316200131008e   1c                   endif
316300131008      *
316400131008      * memorizzo il primo numero di segnacollo
316500131008if  1c                   if        $Mamma = *off
316600131008     c     KLDV          setll     FNART000
316700131008     c     KLDV          reade     FNART000
316800131008x   1c                   else
316900131008     c     KLBL          setll     FNART000
317000131008     c     KLBL          reade     FNART000
317100131008e   1c                   endif
317200131008if  1c                   if        NOT %eof(FNART01L)
317300131008     c                   eval      %subst(Pnsx : 1 : 7) =
317400131008     c                                 %editc(ARTnsc : 'Z')
317500131008e   1c                   endif
317600131008      *
317700131008     c                   ENDSR
317800030515      *
317900030515      *---------------------------------------------------------------*
318000050217      *?Carico LDV da FNARB00F                                       ?*
318100030515      *---------------------------------------------------------------*
318200110208     c     sr_CarARB     BEGSR
318300030515      *
318400110202      /free
318500110202
318600110202         //____________________________________________________________
318700110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
318800110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
318900110202
319000110202         // »? STEP 1 ?«
319100131203         //  ?Verifica se presenti forzature per la distinta           ?
319200131203         //  ?(vedi file FIIRD00F)                                     ?
319300131203
319400131203         //?-?Verifica se distinta in FIIRD10F                           ?
319500131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
319600130716         chain  %kds(k01fiird11)  FIIRD000;
319700130313
319800130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
319900131203           // -?SE rileva errori =>?
320000131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
320100130716           exsr  sr_STEP_1_CtrlTASFLO;
320200130716         EndIf;
320300131203
320400131203         // »? STEP 2 ?«
320500131203         //  ?Verifica se spedizione elaborabile                       ?
320600131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
320700131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
320800110202
320900110204         // -?Reperimento dati distinta (FIDST)?
321000110228         clear  dDstflr;
321100110202         k_DSTnpg = 4;
321200110202         k_DSTnfv = ds_Distinta.p_Ndc;
321300110202         k_DSTfgs = ds_Distinta.p_Fgs;
321400110202         chain  %kds(k03fidst01)  FIDST000;
321500110228         if  %found(FIDST01L);
321600110228           dDSTflr = DSTflr;
321700110228         endif;
321800110202
321900110202         // -?Controllo distinta:?
322000130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
322100130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
322200131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
322300131016         //  ?SE distinta NON chiusa:?
322400131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
322500130716         If  Not %found(FIIRD11L)  and
322600130716             $Test = *blank;
322700131203           exsr  sr_STEP_2_CtrlDISTINTA;
322800130716         EndIf;
322900110202
323000110202
323100110202         // -?Reperimento dati Immagine LdV (FIILV)?
323200110208         clear  ILVcol;
323300110309         clear  wCountRec01;
323400110202         chain(e)  %kds(k06fiilv01)  FIILV000;
323500130313
323600130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
323700131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
323800131016         //  ?SE trovati errori:?
323900131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
324000130716         If  Not  %found(FIIRD11L);
324100131203           exsr  sr_STEP_2_CtrlFIILV;
324200130716         EndIf;
324300130313
324400131203         // »? STEP 3 ?«
324500131203         //  ?Controllo spedizione                                     ?
324600131203         //  ?(da eseguire comunque)                                   ?
324700110202
324800131203         // -?Reperimento dati Spedizione (FNARB)?
324900110207         chain  KLDV  FNARB000;
325000101220
325100110202         // -?Spedizione non trovata?
325200101221         if  NOT %found(FNARB01L);
325300130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
325400131203           //  ?"risponde" ed esce dalla s.p.?
325500110204           Risultato = 'XX';
325600130924           //RtnOpCode = 'FNARB_MANC';
325700130924           //RtnEsito  = -1;
325800130924           // -?Registrazione log in FIILV, differenziando l'errore per?
325900130924           //  ?iter normale (25) o iter con forzatura (52)?
326000130924           if  Not  %found(FIIRD11L);
326100130313             wCod_Log = '25';
326200130716           else;
326300130716             wCod_Log = '52';
326400130716           endif;
326500110208           exsr  sr_WrtFIILV;
326600101221           // -?Uscita dalla s.p.?
326700110204           exsr  sr_End;
326800101221         endif;
326900101221
327000101221
327100110202         // -?Registrazione record per spedizione "OK" (root "buono")?
327200110218         If  wCod_Log = *blank;
327300130206           RootFolder = §ILVimgOk;
327400110203           wCod_Log = '01';
327500110202           exsr  sr_WrtFIILV;
327600110218         EndIf;
327700090121
327800080523      /end-free
327900070514      *
328000070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
328100070514      *   il reperimento dell'eventuale bolla mamma...
328200070514     c                   eval      wB0ntc =  ARBntc
328300120524     c                   eval      wB0dcp =  ARBdcp
328400120524     c                   eval      wB0tmc =  ARBtmc
328500120524     c                   move      ARBpdc        Wpdc4a
328600080523     c                   eval      wB0dcm =  ARBdcm
328700121018     c                   eval      wB0piv =  ARBcpi
328800030515      *
328900030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
329000030515     c                   exsr      sr_DS_ARB
329100030515      *
329200070308      * Decodifico filiale di Arrivo
329300131203     c                   exsr      sr_DecLNA
329400030515      *
329500030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
329600030515      *    "generiche" (3A, TB, 5E)
329700030515     c                   exsr      sr_DS_TAB
329800030515      *
329900030515      * Ultimi dati mancanti:
330000030515      * - Cod. cliente mittente
330100030515if  1c                   If        ARBccm <> *zeros
330200030515     c                   movel     ARBccm        DB0ccm
330300030515x   1c                   Else
330400030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
330500030515     c                   movel     ARBksc        DB0ccm
330600030515e   2c                   endif
330700030515e   1c                   EndIf
330800030515      * - Cod. cliente destinatario
330900030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
331000030515     c                   movel     ARBksc        DB0ccd
331100030515e   1c                   endif
331200030515      * - Se c'è 2ª bolla imposto LNA9999
331300030515if  1c                   if        DB0tb2 <> *blanks
331400030515     c                   eval      DB0ccd = *all'9'
331500030515     c                   movel     ARBlna        DB0ccd
331600030515e   1c                   endif
331700120524      * - Reperimento Data spedizione
331800120524if  1c                   if        DB0mgs <> *zeros
331900120524     c                   z-add     DB0mgs        Wdsp
332000120524     c                   movel     DB0aas        Wdsp
332100120524x   1c                   else
332200120524      *   prendo la data del giorno se non c'è la data spedizione
332300120524     c                   time                    w0140
332400120524     c                   move      w0140         w0080
332500120524     c     *eur          movel     w0080         Data_EUR
332600120524     c     *iso          movel     Data_EUR      Wdsp
332700120524e   1c                   endif
332800030515      *
332900061030      * 3ª impostaz. campi della DS: importi da calcolare
333000061031     c                   exsr      sr_DS_IMPO
333100030515      *
333200080505     c                   ENDSR
333300080724      /free
333400130716
333500130716       //--------------------------------------------------------------
333600130716       //?»  STEP 1  «                                                 ?
333700130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
333800130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
333900130716       //--------------------------------------------------------------
334000130716       BEGSR  sr_STEP_1_CtrlTASFLO;
334100130716
334200130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
334300131016         //  ?della relativa immagine.  Se già presente immagine:?
334400131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
334500131016         //  ?ed esce dalla s.p.?
334600130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
334700130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
334800130716         else;
334900130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
335000130716         endif;
335100130716         if  NOT  %open(TITAS30C);
335200130716           open(e)  TITAS30C;
335300130716           if  %error;
335400130716             exsr  sr_ErrOpenFile;
335500130716           endif;
335600130716         endif;
335700130716
335800130716         chain  %kds(k05titas30 : 4)  TITAS30C;
335900130716
336000130716         If  %found(TITAS30C)            and
336100130716             TASdcm > *zero              and
336200130716             TASndc = ds_Distinta.p_Ndc  and
336300130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
336400130716
336500130716           dTASflo = TASflo;
336600130716
336700130716           if  dTASflo.§FLOiml <> *blank;
336800130716             Risultato = 'XX';
336900130924             //RtnOpCode = 'GIÀ_IMMAG ';
337000130924             //RtnEsito  = -1;
337100130716             // -?Registrazione log in FIILV?
337200130716             wCod_Log = '51';
337300130716             exsr  sr_WrtFIILV;
337400130716             // -?Uscita dalla s.p.?
337500130716             exsr  sr_End;
337600130716           endif;
337700130716
337800130716         EndIf;
337900130716
338000130716       ENDSR;
338100130716
338200130716       //--------------------------------------------------------------
338300131203       //?»  STEP 2  «                                                 ?
338400131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
338500130716       //--------------------------------------------------------------
338600131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
338700130716
338800130716         Select;
338900130716
339000130716           // ·?DISTINTA non trovata => ANOMALIA?
339100130716           When  NOT  %found(FIDST01L)  or
339200130716                 DSTatb <> *blank;
339300130924             // -?Registrazione log in FIILV - se non già fatto?
339400130716             if  wCod_Log = *blank;
339500130716               wCod_Log = '22';
339600130716               exsr  sr_WrtFIILV;
339700130716             endif;
339800130716
339900131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
340000130716           When  DSTfcf = *blank;
340100130716             Risultato  = 'NO';
340200130924             //RtnOpCode  = 'DIST_APERT';
340300130716             //RtnEsito   = -1;
340400130716             // -?Uscita dalla s.p.?
340500130716             exsr  sr_End;
340600130716
340700130716         EndSl;
340800130716
340900130716       ENDSR;
341000130716
341100130716       //--------------------------------------------------------------
341200131203       //?»  STEP 2  «                                                 ?
341300131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
341400130716       //--------------------------------------------------------------
341500131203       BEGSR  sr_STEP_2_CtrlFIILV;
341600130716
341700130716         Select;
341800130716
341900130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
342000130716           When  %error;
342100130716             Risultato  = 'NO';
342200130924             //RtnOpCode  = 'FIILV_ALLC';
342300130716             //RtnEsito   = -1;
342400130716             // -?Uscita dalla s.p.?
342500130716             exsr  sr_End;
342600130716
342700130716           // ·?Rec. IMMAGINE LdV non trovato?
342800130716           When  NOT  %found(FIILV01L);
342900130716             select;
343000130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
343100130716               //  ?=> cancellare immagine (senza LOG)?
343200130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
343300130716                 Risultato  = 'XX';
343400130924                 //RtnOpCode  = 'DIST_NOPDA';
343500130716                 //RtnEsito   = -1;
343600130716                 // -?Uscita dalla s.p.?
343700130716                 exsr  sr_End;
343800130716               // ·?DISTINTA in test consegne?
343900130716               //  ?=> cancellare immagine (senza LOG)?
344000130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
344100130716                 Risultato  = 'XX';
344200130924                 //RtnOpCode  = 'DIST_TEST ';
344300130716                 //RtnEsito   = -1;
344400130716                 // -?Uscita dalla s.p.?
344500130716                 exsr  sr_End;
344600130716               // ·?Altrimenti: ANOMALIA?
344700130924               //  ?Registrazione log in FIILV - se non già fatto?
344800130716               other;
344900130716                 if  wCod_Log = *blank;
345000130716                   wCod_Log = '23';
345100130716                   exsr  sr_WrtFIILV;
345200130716                 endif;
345300130716             endsl;
345400130716
345500130716           // ·?Rec. IMMAGINE LdV già elaborato?
345600130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
345700130716           When  ILVcol <> '00';
345800130716             exsr  sr_CercaLog00;
345900130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
346000130924             //  ?Registrazione log in FIILV - se non già fatto?
346100130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
346200130716                                 and  wCod_Log    = *blank;
346300130716               wCod_Log = '23';
346400130716               exsr  sr_WrtFIILV;
346500130716             endif;
346600130716
346700130716         EndSl;
346800130716
346900130716
347000130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
347100130716         $FIILV = (ILVcol = '00');
347200130716
347300130716         // -?Impostazione struttura dati del campo ILVLIBERO?
347400130716         dILVlibero = ILVlibero;
347500130716
347600130716
347700130716         Select;
347800130716
347900130716           // -?Spedizione non consegnata in distinta?
348000130716           When  ILVnoCDst = 'S';
348100130924             // -?Registrazione log in FIILV - se non già fatto?
348200130716             if  wCod_Log = *blank;
348300130716               wCod_Log = '08';
348400130716               exsr  sr_WrtFIILV;
348500130716             endif;
348600130716
348700130716           // -?LdV da scannerizzare?
348800130716           When  ILVscan = *blank;
348900130924             // -?Registrazione log in FIILV - se non già fatto?
349000130716             if  wCod_Log = *blank;
349100130716               wCod_Log = '09';
349200130716               exsr  sr_WrtFIILV;
349300130716             endif;
349400130716
349500130716         EndSl;
349600130716
349700130716       ENDSR;
349800130716
349900130716       //--------------------------------------------------------------
350000130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
350100130716       //--------------------------------------------------------------
350200130716       BEGSR  sr_CercaLog00;
350300130716
350400130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
350500130716
350600130716         DoU  ILVcol = '00';
350700130716
350800130716           // ·?Rec. Immagine LdV già elaborato ("01")?
350900130716           if  ILVcol = '01';
351000130716             wCountRec01 += 1;
351100130716           endif;
351200130716
351300130716           reade(e)  %kds(k06fiilv01)  FIILV000;
351400130716
351500130716           select;
351600130716
351700130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
351800130716             when  %error;
351900130716               Risultato  = 'NO';
352000130924               //RtnOpCode  = 'FIILV_ALLC';
352100130716               //RtnEsito   = -1;
352200130716               // -?Uscita dalla s.p.?
352300130716               exsr  sr_End;
352400130716
352500130716             // ·?Solo rec. Immagine LdV già elaborati?
352600130716             //  ?=> scrittura nuovo?
352700130716             when  %eof(FIILV01L);
352800130716               clear  ILVcol;
352900130716               leave;
353000130716
353100130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
353200130716             //  ?(questo è in realtà il 101°)?
353300131016             //  ?=> NON ripetere la richiesta (senza LOG)?
353400130716             when  wCountRec01 >= 100;
353500130716               Risultato  = 'XX';
353600130924               //RtnOpCode  = 'SPEDGIÀ100';
353700130716               //RtnEsito   = -1;
353800130716               // -?Invio e-mail?
353900130716               exsr  sr_SendMAIL;
354000130716               // -?Uscita dalla s.p.?
354100130716               exsr  sr_End;
354200130716
354300130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
354400130716             //  ?creazione immagine sul PDA successive all'attuale?
354500131016             //  ?=> NON ripetere la richiesta (senza LOG)?
354600130716             //when  DataOraCrea <= ILVprmTimP;
354700130716             when  DataOraCrea < ILVprmTimP;
354800130716               Risultato  = 'XX';
354900130924               //RtnOpCode  = 'SPED_GIÀEL';
355000130716               //RtnEsito   = -1;
355100130716               // -?Uscita dalla s.p.?
355200130716               exsr  sr_End;
355300130716
355400130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
355500130716             //  ?creazione immagine sul PDA identica all'attuale?
355600130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
355700130716             when  DataOraCrea = ILVprmTimP  and
355800130716                   ILVtimA > %int( %subst( %char( %dec(
355900130716                                   %timestamp() - %minutes(10) ) )
356000130716                                           : 1 : 14 ) );
356100130716               Risultato  = 'NO';
356200130924               //RtnOpCode  = '..TRA10MIN';
356300130716               //RtnEsito   = -1;
356400130716               // -?Uscita dalla s.p.?
356500130716               exsr  sr_End;
356600130716
356700130716           endsl;
356800130716
356900130716         EndDo;
357000130716
357100130716       ENDSR;
357200101221
357300080724       //--------------------------------------------------------------
357400130716       //?Registrazione record nel work-file FIILV00F                  ?
357500080724       //--------------------------------------------------------------
357600110207       BEGSR  sr_WrtFIILV;
357700110202
357800110202         // -?Segnalazione anomalia rilevata per la spedizione?
357900110204         if  Not  $FIILV;
358000110202           clear  FIILV000;
358100110202           ILVfgs = ds_Distinta.p_Fgs;
358200110202           ILVndc = ds_Distinta.p_Ndc;
358300110202           ILVaas = DB0aas;
358400110202           ILVlnp = DB0lnp;
358500110202           ILVnrs = DB0nrs;
358600110202           ILVnsp = DB0nsp;
358700110202         endif;
358800110202
358900110207         ILVcol     = wCod_Log;
359000110202         xx = %lookup( wCod_Log : $CoL );
359100110202         if  xx > *zero;
359200110202           ILVnot   = $DeL(xx);
359300110202         endif;
359400110202         ILVprmSpe  = Spedizione;
359500110202         ILVprmDst  = Distinta;
359600110202         ILVprmTimp = DataOraCrea;
359700110202         ILVrisult  = Risultato;
359800110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
359900130411
360000130411
360100131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
360200130411         IF  $Test <> *blank;
360300130411           leavesr;
360400130411         ENDIF;
360500130411
360600090210
360700110923
360800131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
360900110923         If  SDSprm > 11;
361000110923           leavesr;
361100110923         EndIf;
361200110923
361300110923
361400110204         if  $FIILV;
361500110202           //_______________
361600110202           update  FIILV000;
361700110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
361800110202         else;
361900110204           //_______________
362000110204           write   FIILV000;
362100110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
362200110202         endif;
362300080724
362400080724       ENDSR;
362500130227
362600130227       //--------------------------------------------------------------
362700130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
362800130716       //?   "01" per la stessa spedizione in FIILV.                   ?
362900130227       //--------------------------------------------------------------
363000130227       BEGSR  sr_SendMAIL;
363100130227
363200130227         // -?Reperimento tab. "MRA"?
363300130227         reset  dMRAdan;
363400130227         reset  TIBS02ds;
363500130227         T02cod = 'MRA';
363600130227         T02ke1 = SDSpgm;
363700130227         T02ke2 = *blank;
363800130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
363900130227         if  T02err = *blank;
364000130227           dMRAdan = T02uni;
364100130227         endif;
364200130227
364300130227         // -?Override ai file di stampa?
364400130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
364500130227         exsr sr_OvrPrtF;
364600130227
364700130227         // -?Stampa segnalazione?
364800130227         except  EmlTxt;
364900130227         P_Fgs2 = ds_Distinta.p_Fgs;
365000130227         P_Ndc2 = ds_Distinta.p_Ndc;
365100130227         except  EmlDet;
365200130227         except  EmlEnd;
365300130227
365400130227         // -?Cancellazione Override ai file di stampa?
365500130227         exsr sr_DeleteOvr;
365600130227
365700130227       ENDSR;
365800130227
365900130227       //--------------------------------------------------------------
366000130716       //?Override al file di stampa per impostarvi i dati per         ?
366100130716       //?  l'invio via e-mail.                                        ?
366200130227       //--------------------------------------------------------------
366300130227       BEGSR  sr_OvrPrtF;
366400130227
366500130227         reset  TRTCM1ds;
366600130227         if  §MRAdmitt <> *blank;
366700130227           §CM1mitt = %trim(§MRAdmitt);
366800130227         endif;
366900130227         §CM1dst  = %trim(§MRAddest);
367000130227         §CM1tips = §MRAdreg;
367100130227         //§CM1var  = '*OBJM*' + §MRAddes;
367200130227         §CM1idp  = §MRAdidpro;
367300130227
367400130227         // -?Override al "corpo e-mail"?
367500130227         if  §MRAdreg <> *blank;
367600130227           Qcmd = c_CmdOvrPrtF
367700130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
367800130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
367900130227         else;
368000130227           Qcmd = c_CmdOvrPrtF;
368100130227         endif;
368200130227
368300130227         exsr  sr_ExecCmd;
368400130227
368500130227         //if  Qusei <> *blank;
368600130227         //  ...;
368700130227         //endif;
368800130227
368900130227         // -?Apertura file di stampa?
369000130227         open  PrtEmail;
369100130227
369200130227       ENDSR;
369300130227
369400130227       //--------------------------------------------------------------
369500130227       //?Chiusura file di stampa e cancellazione override.            ?
369600130227       //--------------------------------------------------------------
369700130227       BEGSR  sr_DeleteOvr;
369800130227
369900130227         // -?Chiusura file di stampa?
370000130227         if  %open(PrtEMAIL);
370100130227           close  PrtEMAIL;
370200130227         endif;
370300130227
370400130227         // -?Cancellazione override al file di stampa?
370500130227         Qcmd = c_CmdDltOvr;
370600130227         exsr  sr_ExecCmd;
370700130227
370800130227       ENDSR;
370900130227
371000130227       //--------------------------------------------------------------
371100130227       //?Esecuzione del comando (già impostato).                      ?
371200130227       //--------------------------------------------------------------
371300130227       BEGSR  sr_ExecCmd;
371400130227
371500130227         clear Qcap0100;
371600130227         Qcabcsdh = *off;
371700130227         Qcapa    = *off;
371800130227         Qcacmdss = *off;
371900130227         Qcaerved = *allX'00';
372000130227
372100130227         clear Qusec;
372200130227         Qusbprv  = %size(Qusec);
372300130227
372400130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
372500130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
372600130227                           0 : 0 : Qusec);
372700130227
372800130227         //if  Qusei <> *blank;
372900130227         //  ...;
373000130227         //endif;
373100130227
373200130227       ENDSR;
373300101221
373400080724      /end-free
373500030515      *
373600030515      *---------------------------------------------------------------*
373700050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
373800030515      *---------------------------------------------------------------*
373900030515     c     sr_DS_ARB     BEGSR
374000030515      *
374100030515     c                   eval      DB0cbo = ARBcbo
374200030515     c                   eval      DB0tsp = ARBtsp
374300030515     c                   eval      DB0mgs = ARBmgs
374400030515     c                   eval      DB0tc1 = ARBtc1
374500030515     c                   eval      DB0tc2 = ARBtc2
374600030515     c                   eval      DB0fns = ARBfns
374700030515     c                   eval      DB0rmn = ARBrmn
374800030515     c                   eval      DB0rma = ARBrma
374900030515     c                   eval      DB0ias = ARBias
375000030515     c                   eval      DB0vas = ARBvas
375100030515     c                   eval      DB0vmd = ARBvmd
375200030515     c                   eval      DB0vad = ARBvad
375300030515     c                   eval      DB0qft = ARBqft
375400030515     c                   eval      DB0lna = ARBlna
375500030515     c                   eval      DB0znc = ARBznc
375600030515     c                   eval      DB0rsm = ARBrsm
375700030515     c                   eval      DB0inm = ARBinm
375800030515     c                   eval      DB0cam = ARBcam
375900030515     c                   eval      DB0lom = ARBlom
376000030515     c                   eval      DB0prm = ARBprm
376100030515     c                   eval      DB0nzm = ARBnzm
376200030515     c                   eval      DB0fap = ARBfap
376300030515     c                   eval      DB0rmo = ARBrmo
376400030515     c                   eval      DB0rsd = ARBrsd
376500030515     c                   eval      DB0ind = ARBind
376600030515     c                   eval      DB0cad = ARBcad
376700030515     c                   eval      DB0lod = ARBlod
376800030515     c                   eval      DB0prd = ARBprd
376900030515     c                   eval      DB0nzd = ARBnzd
377000030515     c                   eval      DB0fin = ARBfin
377100030515     c                   eval      DB0gc1 = ARBgc1
377200030515     c                   eval      DB0gc2 = ARBgc2
377300030515     c                   eval      DB0nas = ARBnas
377400030515     c                   eval      DB0ncl = ARBncl
377500030515     c                   eval      DB0pkf = ARBpkf
377600030515     c                   eval      DB0fvf = ARBfvf
377700030515     c                   eval      DB0vlf = ARBvlf
377800030515     c                   eval      DB0fls = ARBfls
377900030515     c                   eval      DB0ncd = ARBncd
378000030515     c                   eval      DB0nca = ARBnca
378100030515     c                   eval      DB0xco = ARBxco
378200030515     c                   eval      DB0ffd = ARBffd
378300030515     c                   eval      DB0dcr = ARBdcr
378400030515     c                   eval      DB0hcr = ARBhcr
378500030515     c                   eval      DB0tcr = ARBtcr
378600030515     c                   eval      DB0fbr = ARBfbr
378700030515     c                   eval      DB0gga = ARBgga
378800030515     c                   eval      DB0gma = ARBgma
378900030515     c                   eval      DB0gva = ARBgva
379000030515     c                   move      ' '           DB0gva
379100030515     c                   clear                   DB0sop
379200030515     c                   eval      DB0ctr = ARBctr
379300030515     c                   clear                   DB0drt
379400030515     c                   clear                   DB0fpp
379500030515     c                   clear                   DB0nrt
379600030515     c                   clear                   DB0pdr
379700030515      *
379800030515      * Allega documento di trasporto
379900060524      * & Codice cliente da Assegnato
380000130924     c***?(già fatto:)?  clear                   dsBL4A
380100031029     c                   clear                   DB0bam
380200061107     c                   exsr      RepAR4_A
380300060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
380400031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
380500030515     c                   movel     'S'           DB0bam
380600060505     c                   endif
380700030515      *
380800030515      * Destinatario: vedo se esiste 2ª ragione sociale
380900061107     c                   clear                   DB0rd2
381000061107     c                   exsr      RepAR4_D
381100030515      *
381200030515      * Note
381300061107     c                   clear                   DB0nt1
381400061107     c                   clear                   DB0nt2
381500061107     c                   exsr      RepAR4_89
381600030515      *
381700030515     c                   ENDSR
381800030515      *
381900030515      *---------------------------------------------------------------*
382000050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
382100050217      *?   "generiche" (non da "1A")                                 ?*
382200030515      *---------------------------------------------------------------*
382300030515     c     sr_DS_TAB     BEGSR
382400030515      *
382500030515      * Carico le tabelle "generiche" (3A, TB, 5E)
382600030515     c                   exsr      CarTAB
382700030515      *
382800030515      * Codici bolla
382900030515     c                   eval      DB0tb1 = §3Atb1
383000030515     c                   eval      DB0tb2 = §3Atb2
383100030515     c                   eval      DB0dcb = §3Ades
383200030515      * Tipo servizio
383300030515if  1c                   if        DB0dts = *blanks
383400030515     c                   eval      DB0dts = §5Edes
383500030515     c                   eval      DB0fst = §5Efst
383600030515e   1c                   endif
383700030515      * Consegne particolari
383800030515if  1c                   if            DB0tc1 <> *blanks
383900030515     c                             and DB0dc1 =  *blanks
384000030515     c                   movel(p)  DB0tc1        TBLkey
384100030515     c                   exsr      Tab_1P
384200030515     c                   eval      DB0dc1 = §1Pdes
384300030515e   1c                   endif
384400030515if  1c                   if            DB0tc2 <> *blanks
384500030515     c                             and DB0dc2 =  *blanks
384600030515     c                   movel(p)  DB0tc2        TBLkey
384700030515     c                   exsr      Tab_1P
384800030515     c                   eval      DB0dc2 = §1Pdes
384900030515e   1c                   endif
385000030515      * Esenzione IVA
385100030515if  1c                   if            DB0cei <> *blanks
385200030515     c                             and DB0dce =  *blanks
385300030515     c                   eval      DB0dce =  §EIdes
385400030515e   1c                   endif
385500030515      *
385600030515     c                   ENDSR
385700060524      *
385800060524      *---------------------------------------------------------------*
385900060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
386000061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
386100060524      *---------------------------------------------------------------*
386200060524     c     RepAR4_A      BEGSR
386300060524      *
386400130924     c***?(già fatto:)?  clear                   dsBL4A
386500061107      *
386600060524     c                   eval      Wtrc = 'A'
386700060524     c     KLDVtrc       chain     FIAR4000
386800060524      *
386900060524if  1c                   if        %found(FIAR401L)
387000060524     c                   movel     AR4Not        dsBL4A
387100060524e   1c                   endif
387200060524      *
387300060524     c                   ENDSR
387400061107      *
387500061107      *---------------------------------------------------------------*
387600061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
387700061107      *?(2ª ragione sociale del destinatario)                        ?*
387800061107      *---------------------------------------------------------------*
387900061107     c     RepAR4_D      BEGSR
388000061107      *
388100130924     c***?NON esiste:?   clear                   dsBL4D
388200130924     c***?(già fatto:)?  clear                   DB0rd2
388300061107      *
388400061107     c                   eval      Wtrc = 'D'
388500061107     c     KLDVtrc       chain     FIAR4000
388600061107      *
388700061107if  1c                   if        %found(FIAR401L)
388800061107     c                   movel     AR4not        DB0rd2
388900061107e   1c                   endif
389000061107      *
389100061107     c                   ENDSR
389200061107      *
389300060626      *---------------------------------------------------------------*
389400060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
389500061107      *?(dati DPD)                                                   ?*
389600060626      *---------------------------------------------------------------*
389700061030     c     RepAR4_I      BEGSR
389800061030      *
389900130924     c***?(già fatto:)?  clear                   dsBL4I
390000061107      *
390100061030     c                   eval      Wtrc = 'I'
390200061030     c     KLDVtrc       chain     FIAR4000
390300061030      *
390400061030if  1c                   if        %found(FIAR401L)
390500061030     c                   movel     AR4not        dsBL4I
390600060626e   1c                   endif
390700061030      *
390800061030     c                   ENDSR
390900071008      *
391000071015      *---------------------------------------------------------------*
391100071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
391200110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
391300071015      *---------------------------------------------------------------*
391400071015     c     RepAR4_M      BEGSR
391500071015      *
391600130924     c***?(già fatto:)?  clear                   dsBL4M
391700071015      *
391800071015     c                   eval      Wtrc = 'M'
391900071015     c     KLDVtrc       chain     FIAR4000
392000071015      *
392100071015if  1c                   if        %found(FIAR401L)
392200071015     c                   movel     AR4Not        dsBL4M
392300071015e   1c                   endif
392400071015      *
392500071015     c                   ENDSR
392600121018      *
392700121018      *---------------------------------------------------------------*
392800121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
392900121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
393000121018      *---------------------------------------------------------------*
393100121018     c     RepAR4_Q      BEGSR
393200121018      *
393300130924     c***?(già fatto:)?  clear                   dTA4Q
393400130924     c***?(già fatto:)?  clear                   DB0cpi
393500121018      *
393600121018     c                   eval      Wtrc = 'Q'
393700121018     c     KLDVtrc       chain     FIAR4000
393800121018      *
393900121018if  1c                   if        %found(FIAR401L)
394000121018     c                   movel     AR4not        dTA4Q
394100121018e   1c                   endif
394200121018      *
394300121018     c                   ENDSR
394400080523      /free
394500080523
394600080523       //--------------------------------------------------------------
394700080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
394800080523       //?(nominativo del destinatario)                                ?
394900080523       //--------------------------------------------------------------
395000080523       BEGSR  RepAR4_1;
395100080523
395200101222         //clear  dsBL41;        *** NON esiste ***?
395300080523
395400080523         Wtrc = '1';
395500080523         chain  KLDVtrc  FIAR4000;
395600080523
395700080523         if  NOT  %found(FIAR401L);
395800080523           clear  AR4not;
395900080523         endif;
396000080523
396100150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
396200081104           Pfedkcn = AR4not;
396300081104         else;
396400081104           Pkcn    = AR4not;
396500081104         endif;
396600080523
396700080523       ENDSR;
396800080523
396900080523      /end-free
397000061107      *
397100061107      *---------------------------------------------------------------*
397200061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
397300061107      *?(note)                                                       ?*
397400061107      *---------------------------------------------------------------*
397500061107     c     RepAR4_89     BEGSR
397600061107      *
397700130924     c***?NON esiste:?   clear                   dsBL48
397800130924     c***?NON usata:?    clear                   dsBL49
397900130924     c***?(già fatto:)?  clear                   DB0nt1
398000130924     c***?(già fatto:)?  clear                   DB0nt2
398100061107      *
398200061107     c                   eval      Wtrc = '8'
398300061107     c     KLDVtrc       chain     FIAR4000
398400061107if  1c                   if        %found(FIAR401L)
398500061107     c                   movel     AR4not        DB0nt1
398600061107e   1c                   endif
398700061107      *
398800061107     c                   eval      Wtrc = '9'
398900061107     c     KLDVtrc       chain     FIAR4000
399000061107if  1c                   if        %found(FIAR401L)
399100061107     c                   movel     AR4not        DB0nt2
399200061107e   1c                   endif
399300061107      *
399400061107     c                   ENDSR
399500030515      *
399600030515      *---------------------------------------------------------------*
399700050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
399800030515      *---------------------------------------------------------------*
399900030515     c     CarTAB        BEGSR
400000030515      *
400100030515      * Tab. "3A" - Codici Bolla
400200030515      * (NO se rimesso mittente)
400300030515     c                   clear                   ds3A
400400030515if  1c                   if        DB0ris <> 'M'
400500030515     c                   movel     '3A'          TBLcod
400600030515     c                   movel(p)  DB0cbo        TBLkey
400700030515     c     K03TAB        chain     TABEL
400800030515if  2c                   if        %found(TABEL00F)
400900030515     c                   movel     TBLuni        ds3A
401000030515e   2c                   endif
401100030515e   1c                   endif
401200070515     c                   eval      $Mamma =  (§3Abcm = 'S')
401300030515      *
401400030515      * Tab. "TB" - Tipi Bolla
401500030515      * (NO se rimesso mittente)
401600030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
401700030515      * se no non stampo i dati fattura
401800030515     c                   clear                   dsTB
401900030515if  1c                   if        DB0ris <> 'M'
402000030515     c                   movel     'TB'          TBLcod
402100121029if  2c                   if        §3Atb2 <> *blanks
402200030515     c                   movel(p)  §3Atb2        TBLkey
402300030515x   2c                   else
402400030515     c                   movel(p)  §3Atb1        TBLkey
402500030515e   2c                   endif
402600030515     c     K03TAB        chain     TABEL
402700030515if  2c                   if        %found(TABEL00F)
402800030515     c                   movel     TBLuni        dsTB
402900030515e   2c                   endif
403000030515e   1c                   endif
403100030515      *
403200030515      * Tab. "5E" - Tipi Servizio
403300030515     c                   clear                   ds5E
403400090514if  1c                   if        DB0tsp <> *blanks
403500030515     c                   movel     '5E'          TBLcod
403600030515     c                   movel(p)  DB0tsp        TBLkey
403700030515     c     K03TAB        chain     TABEL
403800030515if  2c                   if        %found(TABEL00F)
403900030515     c                   movel     TBLuni        ds5E
404000030515e   2c                   endif
404100030515e   1c                   endif
404200030515      *
404300030911      * Tab. "EI" - Esenzioni I.V.A.
404400030515     c                   clear                   dsEI
404500030911      * Tab. "1A" - Tipi incasso contrassegno
404600030911     c                   clear                   ds1A
404700030911      * Tab. "1P" - Consegne particolari
404800030911     c                   clear                   ds1P
404900030515      *
405000030515     c                   ENDSR
405100030911      *
405200030911      *---------------------------------------------------------------*
405300050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
405400030911      *---------------------------------------------------------------*
405500030911     c     Tab_EI        BEGSR
405600030911      *
405700030911     c                   clear                   dsEI
405800030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
405900030911     c                   movel     'EI'          TBLcod
406000030911     c                   movel(p)  DB0cei        TBLkey
406100030911     c     K03TAB        chain     TABEL
406200030911if  2c                   if        %found(TABEL00F)
406300030911     c                   movel     TBLuni        dsEI
406400030911e   2c                   endif
406500030911e   1c                   endif
406600030911     c                   eval      DB0dce =  §EIdes
406700030911      *
406800030911     c                   ENDSR
406900030515      *
407000030515      *---------------------------------------------------------------*
407100050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
407200030515      *---------------------------------------------------------------*
407300030515     c     Tab_1A        BEGSR
407400030515      *
407500030515     c                   clear                   ds1A
407600030515if  1c                   if        DB0cas <> *zeros
407700030515     c                   movel     '1A'          TBLcod
407800030515     c                   movel(p)  DB0tic        TBLkey
407900030515     c     K03TAB        chain     TABEL
408000030515if  2c                   if        %found(TABEL00F)
408100030515     c                   movel     TBLuni        ds1A
408200030515e   2c                   endif
408300030515e   1c                   endif
408400030515      *
408500030515      * Imposto i dati nella DS
408600030515     c                   eval      DB0dti = §1Ades
408700030515     c                   eval      DB0ucm = §1Aucm
408800030515     c                   eval      DB0ste = §1Aste
408900030515      *
409000030515     c                   ENDSR
409100030515      *
409200030515      *---------------------------------------------------------------*
409300050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
409400030515      *---------------------------------------------------------------*
409500030515     c     Tab_1P        BEGSR
409600030515      *
409700030515     c                   clear                   ds1P
409800030515     c                   movel     '1P'          TBLcod
409900130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
410000030515     c     K03TAB        chain     TABEL
410100030515if  1c                   if        %found(TABEL00F)
410200030515     c                   movel     TBLuni        ds1P
410300030515e   1c                   endif
410400030515      *
410500030515     c                   ENDSR
410600030515      *
410700030515      *---------------------------------------------------------------*
410800050217      *?Calcolo importi LDV                                          ?*
410900030515      *---------------------------------------------------------------*
411000061031     c     sr_DS_IMPO    BEGSR
411100030515      *
411200030515      * Vedo se esiste tassazione
411300030515      *  solo se tipo bolla contabilizzabile
411400061030if  1c                   IF        §TBfcb =  '1'
411500101213      *
411600061030     c                   eval      Wtrc   =  '1'
411700040607     c     KLDVtrc       chain     FIAR6000
411800101213      *
411900030515if  2c                   if        %found(FIAR601L)
412000030515     c                   movel     AR6div        DB0div
412100030515     c                   exsr      CtrDIV
412200030515      *
412300030515      * NON c'è 2ª bolla
412400101213if  3c                   if        DB0tb2 = *blanks
412500030515      *  Estremi fattura
412600030515     c                   z-add     AR6nft        DB0nft
412700030515     c                   z-add     AR6dft        DB0dft
412800130205     c                   z-add     AR6fiv        wB0fiv
412900101213      *  Se è assegnato => stampo tutta la tassaz.
413000101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
413100061030     c                   eval      Wtrc   =  '1'
413200030515     c                   exsr      RepTASSAZ
413300030515e   4c                   endif
413400030515e   3c                   endif
413500030515e   2c                   endif
413600030515      *
413700061030      * C'è 2ª bolla
413800101213if  2c                   IF        DB0tb2 <> *blanks
413900030515      *
414000061030     c                   eval      Wtrc   =  '2'
414100040607     c     KLDVtrc       chain     FIAR6000
414200030515      *
414300061030if  3c                   If        %found(FIAR601L)
414400030515     c                   movel     AR6div        DB0div
414500030515     c                   exsr      CtrDIV
414600030515     c                   movel     AR6cei        DB0cei
414700030515      *   Estremi fattura
414800030515     c                   z-add     AR6nft        DB0nft
414900030515     c                   z-add     AR6dft        DB0dft
415000130205     c                   z-add     AR6fiv        wB0fiv
415100030515if  4c                   if        AR6ksc <> *zeros
415200030515     c                   eval      DB0ccd =  AR6ksc
415300030515e   4c                   endif
415400061030     c                   eval      Wtrc   =  '2'
415500030515     c                   exsr      RepTASSAZ
415600061030e   3c                   EndIf
415700030515      *
415800030515e   2c                   ENDIF
415900030515      *
416000030515e   1c                   ENDIF
416100030515      *
416200030515      * Vedo se ha contrassegno
416300060505     c     KLDV          chain     FIAR9000
416400060505if  1c                   IF        %found(FIAR901L)
416500061030     c                   eval      DB0cas =  AR9cas
416600061030     c                   eval      DB0vca =  AR9vca
416700061030     c                   eval      DB0gca =  AR9gca
416800061030     c                   eval      DB0tic =  AR9tic
416900030515x   1c                   ELSE
417000030515     c                   clear                   DB0cas
417100030515     c                   clear                   DB0vca
417200030515     c                   clear                   DB0gca
417300030515     c                   clear                   DB0tic
417400030515     c                   clear                   DB0dti
417500030515     c                   clear                   DB0ucm
417600030515     c                   clear                   DB0ste
417700030515e   1c                   ENDIF
417800121018      *
417900131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
418000121018      *   (NON se destinatario estero)
418100121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
418200121018     c                             and (DB0nft  <> *zeros
418300121018     c                              or  DB0ift  <> *zeros)
418400121018     c                             and  DB0nzd  =  *blanks )
418500121018     c                   exsr      RepAR4_Q
418600121018     c                   movel     §TA4Qcfd      DB0cpi
418700121018e   1c                   endif
418800121018     c                   eval      wB0cfi = DB0cpi
418900061030      *
419000030515      * Vedo se si tratta di bolla figlia:
419100040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
419200070515if  1c                   if        $Mamma =  *on
419300030911     c                   exsr      BollaFiglia
419400030515e   1c                   endif
419500030515      *
419600030515     c                   ENDSR
419700030515      *
419800030515      *---------------------------------------------------------------*
419900050217      *?Decodifica LNA                                               ?*
420000030515      *---------------------------------------------------------------*
420100131203     c     sr_DecLNA     BEGSR
420200030515      *
420300030515if  1c                   if            DB0dla =  *blanks
420400030515     c                             and DB0ris <> 'M'
420500030515      *
420600040311     c                   eval      SAVlna = DB0lna
420700070312     c                   clear                   OG146arr
420800061016     c                   clear                   OG148arr
420900070312     c                   clear                   OG150arr
421000030515     c     DB0lna        chain     AZORG
421100030515      *
421200030515if  2c                   if        %found(AZORG01L)
421300030515     c                   eval      DB0fag = ORGfag
421400030515     c                   eval      DB0dla = ORGdes
421500030515     c                   eval      DB0inl = ORGind
421600030515     c                   eval      DB0cal = ORGcpf
421700030515     c                   eval      DB0lol = ORGloc
421800030515     c                   eval      DB0prl = ORGpro
421900030515     c                   eval      DB0tel = ORGtel
422000030515     c                   eval      DB0fax = ORGfax
422100070312     c                   movel     ORGde6        OG146arr
422200061016     c                   movel     ORGde8        OG148arr
422300070312     c                   movel     ORGdf0        OG150arr
422400030515e   2c                   endif
422500030515      *
422600030515e   1c                   endif
422700030515      *
422800030515     c                   ENDSR
422900030515      *
423000030515      *---------------------------------------------------------------*
423100050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
423200030515      *---------------------------------------------------------------*
423300030515     c     RepTASSAZ     BEGSR
423400030515      *
423500030515     c                   eval      DB0imv = AR6imv
423600030515     c                   eval      DB0cei = AR6cei
423700030515     c                   eval      DB0ift = AR6ift
423800101213      *
423900030911      * Esenzione IVA
424000030911if  1c                   if            DB0cei <> *blanks
424100030911     c                             and DB0dce =  *blanks
424200030911     c                   exsr      Tab_EI
424300030911e   1c                   endif
424400101213      *
424500101213      * Carico tabella VARIABILI ARRIVI
424600101213     c                   clear                   ds$2
424700101213     c                   eval      TBLcod = '$2'
424800101213     c                   eval      TBLkey = '1'
424900101213     c     K03TAB        chain     TABEL
425000101213if  1c                   if        %found(TABEL00F)
425100101213     c                   movel     TBLuni        ds$2
425200101213e   1c                   endif
425300030515      *
425400030515      * Controllo se ci sono varie esenti
425500030515      *
425600030515      * - varia 1
425700030515     c                   eval      Wsvx = AR6sv1
425800030515     c                   eval      Wvax = AR6va1
425900030515     c                   exsr      RepTASSAZ_b
426000030515      * - varia 2
426100030515     c                   eval      Wsvx = AR6sv2
426200030515     c                   eval      Wvax = AR6va2
426300030515     c                   exsr      RepTASSAZ_b
426400030515      * - varia 3
426500030515     c                   eval      Wsvx = AR6sv3
426600030515     c                   eval      Wvax = AR6va3
426700030515     c                   exsr      RepTASSAZ_b
426800030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
426900040607     c     KLDVtrc       setll     FIAR7000
427000040607     c     KLDVtrc       reade     FIAR7000
427100030515      *
427200030515do  2c                   Dow       Not %eof(FIAR701L)
427300030515      *
427400030515     c                   eval      Wsvx = AR7svn
427500030515     c                   eval      Wvax = AR7van
427600030515     c                   exsr      RepTASSAZ_b
427700030515      *
427800040607     c     KLDVtrc       reade     FIAR7000
427900030515e   2c                   Enddo
428000030515      *
428100030515     c                   ENDSR
428200030515      *
428300030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
428400050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
428500050217      *?- 2ª parte -                                                 ?*
428600030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
428700030515     c     RepTASSAZ_b   BEGSR
428800030515      *
428900030515sel 1c                   Select
429000030515      *
429100030515w   1c                   When      Wsvx       = *blanks
429200030515      *
429300030515w   1c                   When      Wsvx       = §$2iva
429400030515     c                   eval      DB0iva     = Wvax
429500030515      *
429600030515w   1c                   When      Wsvx       = §$2bol
429700030515     c                   add       Wvax          DB0ese
429800110117     c*//                z-add     Wvax          wBolloVirt
429900110117     c*//                eval      $BolloVirt = *on
430000030515      *
430100030515x   1c                   Other
430200101213      * Reperimento varie esenti IVA
430300101213     c                   movel     'CC'          TBLcod
430400101213     c                   movel(p)  Wsvx          TBLkey
430500101213     c     K03TAB        chain     TABEL
430600101213if  1c                   IF             %found(TABEL00F)
430700101213     c                             and  TBLflg = *blanks
430800101213     c                   movel     TBLuni        dsCC
430900101213if  2c                   if        §CCcei <> *blanks
431000101213     c                   add       Wvax          DB0ese
431100101213     c                   sub       Wvax          DB0imv
431200101213e   2c                   endif
431300101213e   1c                   ENDIF
431400030515      *
431500101213e   1c                   Endsl
431600030515      *
431700030515     c                   ENDSR
431800030515      *
431900030515      *---------------------------------------------------------------*
432000050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
432100030515      *---------------------------------------------------------------*
432200030515     c     TABxPAR       BEGSR
432300030515      *
432400100517     c                   movel(p)  WCdPar        w053a
432500030515     c                   movel     '7V'          TBLcod
432600030515     c                   movel(p)  KeyPAR        TBLkey
432700030515     c     K03TAB        chain     TABEL
432800030515      *
432900030515if  1c                   if        %found(TABEL00F)
433000030515     c                             and TBLflg = *blanks
433100100517     c                   eval      w053a = WCdPar + '-'
433200121108     c                                   + %trim(%subst(TBLuni:1:49))
433300030515x   1c                   else
433400030515     c                   clear                   TBLuni
433500030515e   1c                   endif
433600030515      *
433700030515     c                   ENDSR
433800030515      *
433900030515      *---------------------------------------------------------------*
434000050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
434100030515      *---------------------------------------------------------------*
434200030515     c     CtrCAs        BEGSR
434300030515      *
434400030515      * E' da convertire
434500030515if  1c                   if        DB0ucm = 'S'
434600030515     c                   eval      $CAeur = *on
434700030515e   1c                   endif
434800030515      *
434900030515      * Se in EURO è da convertire e da stampare in EURO
435000030515if  1c                   if        DB0vca = §GEDcn
435100030515     c                   eval      $CAeur = *on
435200030515     c                   endif
435300030515      *
435400030515      * Devo convertire il c/a da valuta a moneta di conto
435500030624if  1c                   if        DB0vca <> §GEDcn
435600030515     c                   clear                   YEURCODS
435700030515     c                   movel     DB0vca        YECdvi
435800030515     c                   z-add     DB0cas        YECimi
435900030515     c                   movel     §GEDcn        YECdvo
436000030515     c                   call      'YEURCO'
436100030515     c                   parm                    YEURCODS
436200030515if  2c                   if        YECesi = *blanks
436300030515     c                   z-add     YECimo        Wcas
436400030515e   2c                   endif
436500030515x   1c                   else
436600030515     c                   z-add     DB0cas        Wcas
436700030515e   1c                   endif
436800030515      *
436900030515     c                   ENDSR
437000030515      *
437100030515      *---------------------------------------------------------------*
437200050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
437300030515      *---------------------------------------------------------------*
437400030515     c     CtrDIV        BEGSR
437500030515      *
437600030515     c                   clear                   dsCV
437700030515      *
437800030515     c                   movel     'CV'          TBLcod
437900030515     c                   movel(p)  DB0div        TBLkey
438000030515     c     K03TAB        chain     TABEL
438100030515     c                   if        %found(TABEL00F)
438200030515     c                   movel     TBLuni        dsCV
438300030515     c                   movel     §CVfdc        DB0fdc
438400030515     c                   endif
438500030515      *
438600030515     c                   ENDSR
438700030515      *
438800030515      *---------------------------------------------------------------*
438900050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
439000030515      *---------------------------------------------------------------*
439100030911     c     BollaFiglia   BEGSR
439200030515      *
439300090619     c                   if        not %open(FNLBL01L)
439400101213     c                   open(e)   FNLBL01L
439500101213      /free
439600101213           if  %error;
439700101213             exsr  sr_ErrOpenFile;
439800101213           endif;
439900101213      /end-free
440000090619     c                   endif
440100090619      *
440200040607     c     KLDV          chain     FNLBL000
440300030515if  1c                   if        %found(FNLBL01L)
440400030515      *
440500030515     c     KLBL          chain     FNARB000
440600030515if  2c                   if        %found(FNARB01L)
440700030515      *
440800031015     c                   eval      wB0nrs = ARBnrs
440900030515     c                   eval      DB0fns = ARBfns
441000030515     c                   eval      DB0fls = ARBfls
441100030515     c                   eval      DB0ncd = ARBncd
441200030515     c                   eval      DB0nca = ARBnca
441300050114     c**!!!              eval      DB0xco = ARBxco
441400030515     c                   eval      DB0znc = ARBznc
441500030515      * Aggancio AR9
441600060505     c     KLBL          chain     FIAR9000
441700060505if  3c                   IF        %found(FIAR901L)
441800030515     c                   eval      DB0cas = AR9cas
441900030515     c                   eval      DB0vca = AR9vca
442000030515     c                   eval      DB0gca = AR9gca
442100030515     c                   eval      DB0tic = AR9tic
442200030515x   3c                   ELSE
442300030515     c                   clear                   DB0cas
442400030515     c                   clear                   DB0vca
442500030515     c                   clear                   DB0gca
442600030515     c                   clear                   DB0tic
442700030515     c                   clear                   DB0dti
442800030515     c                   clear                   DB0ucm
442900030515     c                   clear                   DB0ste
443000030515e   3c                   ENDIF
443100030515      *
443200030515e   2c                   endif
443300030515      *
443400030515e   1c                   endif
443500030515      *
443600030515     c                   ENDSR
443700030903      *
443800030515      *---------------------------------------------------------------*
443900050217      *?DEFINIZIONE KEY-LIST                                         ?*
444000030515      *---------------------------------------------------------------*
444100030515     c     DefKLIST      BEGSR
444200030515      *
444300090619      * - FNARB01L / FNART01L /
444400030515      *   FNLBL01L /
444500060505      *   FIAR501L/31C / FIAR901L
444600040607     c     KLDV          klist
444700030515     c                   kfld                    DB0aas
444800030515     c                   kfld                    DB0lnp
444900030515     c                   kfld                    DB0nrs
445000030515     c                   kfld                    DB0nsp
445100060505      * - FIAR401L / FIAR601L / FIAR701L
445200060505     c     KLDVtrc       klist
445300060505     c                   kfld                    DB0aas
445400060505     c                   kfld                    DB0lnp
445500060505     c                   kfld                    DB0nrs
445600060505     c                   kfld                    DB0nsp
445700060505     c                   kfld                    Wtrc
445800040607      * - FIAR501L/31C
445900030717     c     K05AR501      klist
446000030717     c                   kfld                    DB0aas
446100030717     c                   kfld                    DB0lnp
446200030717     c                   kfld                    DB0nrs
446300030717     c                   kfld                    DB0nsp
446400030717     c                   kfld                    AR5trd
446500090619      * - FNARB01L / FIAR901L di bolla MAMMA
446600070514     c     KLBL          klist
446700070514     c                   kfld                    LBLaap
446800070514     c                   kfld                    LBLlpp
446900070514     c                   kfld                    LBLnrp
447000070514     c                   kfld                    LBLnsp
447100081030      * - FNART01L di bolla MAMMA
447200081030     c     KLBLnsc       Klist
447300081030     c                   kfld                    LBLaap
447400081030     c                   kfld                    LBLlpp
447500081030     c                   kfld                    LBLnrp
447600081030     c                   kfld                    LBLnsp
447700081030     c                   Kfld                    Wnsc
447800081030     c     K05ARS01A     klist
447900081030     c                   kfld                    ARTfls
448000081030     c                   kfld                    ARTlna
448100081030     c                   kfld                    ARTnrs
448200081030     c                   kfld                    ARTnsc
448300081030     c                   kfld                    ARStrc
448400081030      * - FNART01L
448500081030     c     KLDVnsc       Klist
448600081030     c                   Kfld                    DB0aas
448700081030     c                   Kfld                    DB0lnp
448800081030     c                   Kfld                    DB0nrs
448900081030     c                   Kfld                    DB0nsp
449000081030     c                   Kfld                    Wnsc
449100100517      * - FNORM01L
449200100517     c     K04ORM01      klist
449300100517     c                   kfld                    ORMpoe
449400100517     c                   kfld                    ORMnsr
449500100517     c                   kfld                    ORMnor
449600100517     c                   kfld                    ORMnrv
449700030515      * - TABEL00F
449800030515     c     K03TAB        klist
449900040223     c                   kfld                    TBLkut
450000030515     c                   kfld                    TBLcod
450100030515     c                   kfld                    TBLkey
450200030515      *
450300030515     c                   ENDSR
450400080430
450500080430       //--------------------------------------------------------------
450600140303       //?Operazioni finali                                            ?
450700080430       //--------------------------------------------------------------
450800080430     c     sr_End        BEGSR
450900080430      *
451000080430     c                   clear                   TIBS02ds
451100080430     c                   eval      T02tla = 'C'
451200080430     c                   call      'TIBS02R'
451300080430     c                   parm                    KPJBA
451400080430     c                   parm                    TIBS02ds
451500080430     c                   reset                   FNLV80ds
451600080430     c                   call      'FNLV80R'
451700080430     c                   parm                    KPJBA
451800080430     c                   parm                    FNLV80ds
451900080430      /free
452000101222         if  §VPOrcm <= wDate and
452100101222             DB0tsp   = 'E'   and
452200101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
452300101222                                   DB0lnp <> 006   and
452400101222                                   DB0lnp <> 063));
452500101222           reset  TNSD99ds;
452600101222           tnsd99r ( TNSD99ds );
452700110117         endif;
452800080724
452900101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
453000090210
453100110202         if  %open(FIILV01L);
453200110202           close  FIILV01L;
453300090210         endif;
453400080430
453500101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
453600080508
453700110221         if wCod_Log  <> '01';
453800130206           RootFolder = §ILVimgSca;
453900110221         endif;
454000110221
454100080520         if Risultato <> 'OK';
454200080718
454300080731           clear FileName;
454400080731           clear TemplateName;
454500080508
454600130213           for wx = 1 to c_Row;
454700130206             %occur(LdV) = wx;
454800130206             LdvPrg      = wx;
454900130206           endfor;
455000080508
455100080508         endif;
455200080430
455300131203
455400080430           exec sql   set result sets
455500080430                      for return to client
455600081103                      array :LdV   for :w_row rows;
455700080430
455800080430           select;
455900080430             when sqlCode = 100;
456000080430               RtnStatus  = sqlCode;
456100130924               //RtnOpCode  = 'SQLCODE   ';
456200130924               //RtnEsito   = -1;
456300080430             when sqlCode < *zero;
456400080430               RtnStatus  = sqlCode;
456500130924               //RtnOpCode  = 'SQLCODE   ';
456600130924               //RtnEsito   = -1;
456700080430           endsl;
456800080430
456900080430
457000080430         return;
457100080430
457200080430      /end-free
457300080430     c                   ENDSR
457400080430
457500080430      *---------------------------------------------------------------*
457600130227
457700140303       //--------------------------------------------------------------
457800140303       //?Spool di stampa (per e-mail).                                ?
457900140303       //--------------------------------------------------------------
458000130227
458100130227     oPrtEMAIL  e            EmlTxt            2
458200130227     o                       SDSpgm
458300130227     o                                        +   1 '-'
458400130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
458500130227     o                                              ABORATA 100 VOLTE'
458600130227     o          e            EmlTxt      0
458700130227     o                       SDSpgm
458800130227     o                                        +   1 '-'
458900130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
459000130227     o                                              ABORATA 100 VOLTE'
459100130227     o          e            EmlTxt      2
459200130227     o                                              'Spedizione     '
459300130227     o                                        +   3 'Distinta  '
459400130227     o          e            EmlTxt      1  1
459500130227     o                                              '------------------
459600130227     o                                              --'
459700130227     o                                        +   3 '----------'
459800130227     o          e            EmlDet      1
459900130227     o                       DB0aas        3
460000130227     o                       DB0lnp        3  +   1
460100130227     o                       DB0nrs        3  +   1
460200130227     o                       DB0nsp        3  +   1
460300130227     o                       P_Fgs2        3  +   3
460400130227     o                       P_Ndc2        3  +   1
460500130227     o          e            EmlEnd      2
460600130227     o                                              '***  Fine Lista  ***'
460700130227     o          e            EmlEnd      0
460800130227     o                                              '***  Fine Lista  ***'
460900080724
461000140303       //--------------------------------------------------------------
461100140303       //?Spool per immagine della LdV.                                ?
461200140303       //--------------------------------------------------------------
461300080430     oQSYSPRT   e            PRTtxt            3
461400080430     o                                              '   '
461500080430     o                       c_Char           +   0
461600080430     o          e            PRTrow      1
461700080430     o                       LDV              +   0
461800110201     o          e            PRTend      3
461900110201     o                       LastRow          +   0
462000080430
462100080718       //--------------------------------------------------------------
462200140303       //?Procedure name: getCurrentSystemName.                        ?
462300140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
462400140303       //?Returns:        Current System Name                          ?
462500080718       //--------------------------------------------------------------
462600080718     p getCurrentSystemName...
462700080718     p                 b
462800080718
462900080718     d getCurrentSystemName...
463000080718     d                 pi             8a
463100080718
463200080718      /free
463300080718
463400080718       clear qUSEC;
463500080718       qUSBPRV = %size(qUSEC);
463600080718
463700080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
463800080718                                : %size(qWCRNETA_dataReturned)
463900080718                                : 1
464000080718                                : 'SYSNAME   '
464100080718                                : qUSEC
464200080718                                );
464300080718
464400080718       if  qUSEI <> *blank   or
464500080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
464600080718         dump(A);
464700080718         clear qWCRNETA_networkAttributeInformationTable;
464800080718       else;
464900080718         qWCRNETA_networkAttributeInformationTable =
465000080718                                       qWCRNETA_dataReturned.qWCARRAY;
465100080718       endif;
465200080718
465300080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
465400080718
465500080718      /end-free
465600080718
465700080718     p getCurrentSystemName...
465800080718     p                 e
465900080718
466000110202       //--------------------------------------------------------------
466100110202       //?Schiere a tempo di compilazione.                             ?
466200110202       //--------------------------------------------------------------
466300030515
466400030515** > $GgS/$Gio * --------------------------------------------------------------*
4665000306241LUNEDI
4666000306242MARTEDI
4667000410263MERCOLEDI
4668000306244GIOVEDI
4669000306245VENERDI
4670000305156SABATO
4671000305157DOMENICA
467200030515M          MATT.
467300030515P          POM.
467400110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
46750011020200Spedizione in distinta chiusa - da elaborare
46760011020201Spedizione elaborata
46770011032808Sped. non consegnata in dist. - immagine a SCARTO
46780011032509LdV da scannerizzare - immagine a SCARTO
46790011020321Parametri di input errati - immagine NON creata
46800011020822Distinta non trovata/annullata - immagine a SCARTO
46810011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
46820011020325Sped. non trovata in FNARB - immagine NON creata
46830013031351BOLLA RECUPERO - Immagine già generata
46840013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
