000100100827     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'TRUL') ACTGRP('BARTVAS')
000200100826     H decedit('0,') datedit(*ymd/) option(*nodebugio)
000300990901     F*------------------------------------------------------------------------*
000400990901     F* DATA BASE
000500990901     F*------------------------------------------------------------------------*
000600100827     Fsgius     IF   E           K DISK
000700100827     FTIVSS01L  IF   E           K DISK
000800100826     FPRTF198   O    F  198        PRINTER usropn
000900001005     F*------------------------------------------------------------------------*
001000100827     D  wRangeBAK      S              3S 0 inz
001100100826     D  datcor         S              8S 0 inz
001200100827     D  wDatScad       S              8S 0 inz
001300100826     D*------------------------------------------------------------------------*
001400100827
001500100827     D*-------------------
001600100827     D* DEFINIZIONI PROCEDURE ESTERNE
001700100827     D*-------------------
001800100827     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
001900100827     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
002000100827     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
002100100827     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
002200100827
002300100827
002400100826     c
002500100826      *
002600100826     c     *Entry        plist
002700100826     c                   parm                    IN_ELA            1
002800100827     c                   parm                    IN_GG             3
002900100826      *
003000100826      * Verifico se lancio in aggiornamento
003100100826     c                   if        IN_ELA = 'A'
003200100826     c                   seton                                        55
003300100826     c                   else
003400100826     c                   setoff                                       55
003500100826     c                   endif
003600100827      *
003700100827      * Calcolo la data scadenza servizii prima della quale eliminare l'abilitazione
003800100827     c                   move(p)   IN_GG         wRangeBAK
003900100827     c                   eval      wDatScad = %dec(%date(datcor)-
004000100827     c                                        %days(wRangeBAK))
004100100826      *
004200100826      * Inizializzazioi contatori d wrk
004300100826     c                   z-add     *zeros        wContaUPD         6 0
004400100826     c                   z-add     *zeros        wContaTOT         6 0
004500100826      *
004600100826      * Inizializzo riga d stampa
004700100826     c                   movel     *blanks       wRigaPrt        150
004800100826      *
004900100826      * Apro il printer file
005000100826     c                   open      PRTF198
005100100826      *
005200100826     c                   if        IN_ELA = 'A'
005300100827     c                   eval      wRigaPrt='TIS7VSS2R - ' +
005400100827     c                                      '* Elaborazione in AGGIORNAMENTO *'
005500100826     c                   else
005600100827     c                   eval      wRigaPrt='TIS7VSS2R - ' +
005700100827     c                                      '* Elaborazione in SIMULAZIONE *'
005800100826     c                   endif
005900100826     c                   except    staLista
006000100827     c                   eval      wRigaPrt = 'Eliminazione disabilitati ' +
006100100827     c                                        'prima del ' + %char(wDatScad)
006200100827     c                   except    staLista
006300100826     c                   eval      wRigaPrt=''
006400100826     c                   except    staLista
006500100826      *
006600100827     c     *start        setll     sgius
006700100827     c                   read      sgius
006800100827     c                   dow       not %eof(sgius)
006900100827      *
007000100827      * Inizializzazione variabili d wrk
007100100827     c                   add       1             wContaTOT
007200100827      *
007300100827      * Elaboro solo se nome utente è un numerico d 7 byte
007400100827     c                   if        %len(%trim(usANAM)) = 7
007500100827     c                   eval      PiStr=%trim(usANAM)
007600100827     c                   exsr      CHKNUM
007700100827     c                   if        PiInt=*on
007800100827      *
007900100827      * Verifico se trattasi d utente SGI "NON ABILITATO"
008000100827     c                   if        usSTAT <> '1'
008100100827      *
008200100827     c                   setoff                                       60
008300100827      *
008400100827      * Verifico la data d scadenza del abilitazione su TIVSS, se antecedente => procedo
008500100827     c                   eval      vssSUN = usUSER
008600100827     c                   eval      vssISV = 'TT'
008700100827     c     KEY_vss01_C   chain     tivss01l
008800100827     c                   if        %found(tivss01l)
008900100827     c                   if        vssDSC < wDatScad
009000100827      *
009100100827      * Se abilitazione e trovata e scaduta almeno prima della data d riferimento => PROCEDO
009200100827     c                   seton                                        60
009300100827     c                   endif
009400100827     c                   else
009500100827      * Se nn trovata abilitazione => PROCEDO
009600100827     c                   seton                                        60
009700100827     c                   endif
009800100827      *
009900100827      * Se OK a procedere
010000100827     c                   if        *in60
010100100826      *
010200100826      * Listo utente / cliente corrente
010300100827     c                   eval      wRigaPrt = ' Utente SGI: '   + usUSER +
010400100827     c                                        ' Nome utente: '  + usANAM
010500100826     c                   except    staLista
010600100826     c                   add       1             wContaUPD
010700100826      *
010800100826      * Se lancio in aggiornamento => procedo effettivamente alla modifica
010900100826     c                   if        *in55
011000100827     c                   exsr      CHKSGI
011100100826     c                   endif
011200100827      *
011300100827     c                   endif
011400100827     c                   endif
011500100827     c                   endif
011600100827     c                   endif
011700100826      *
011800100827      * Proseguo con la lettura sino a fine file
011900100827     c                   read      sgius
012000100826     c                   enddo
012100100826      *
012200100826      * Stampa totali finali
012300100826     c                   eval      wRigaPrt = *blanks
012400100826     c                   except    staLista
012500100827     c                   eval      wRigaPrt = 'TOT SGI USER ELIM: '+
012600100826     c                                         %editc(wContaUPD:'X')
012700100826     c                   except    staLista
012800100827     c                   eval      wRigaPrt = 'TOT SGI USER ELAB: '+
012900100826     c                                         %editc(wContaTOT:'X')
013000100826     c                   except    staLista
013100100826      *
013200100826      * Chiudo il printer file
013300100826     c                   close     PRTF198
013400100826      *
013500100826     c                   SETON                                        LR
013600100826     C*------------------------------------------------------------------------*
013700100827
013800100827
013900100827
014000100827     C*------------------------------------------------------------------------*
014100100827     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
014200100827     C*------------------------------------------------------------------------*
014300100827     C     CHKSGI        BEGSR
014400100827      /FREE
014500100827               if UBUTISGI_Init = 0;
014600100827               endif;
014700100827      /END-FREE
014800100827     C*
014900100827      /FREE
015000100827               pInSUN = usUSER;
015100100827
015200100827               if UBUTISGI_DeleteSgiUsr(pInSUN:pOutEsito) = 0;
015300100827               endif;
015400100827
015500100827      /END-FREE
015600100827     C
015700100827     C
015800100827      /FREE
015900100827               if UBUTISGI_Finalize = 0;
016000100827               endif;
016100100827      /END-FREE
016200100827     C*
016300100827     C                   ENDSR
016400100827
016500100827
016600100827
016700100827     C*----------------------------------------------------*
016800100827     C*  CONTROLLO NUMERICITA' CAMPI
016900100827     C*----------------------------------------------------*
017000100827     C     CHKNUM        BEGSR
017100100827     C*
017200100827     C                   callp     UBISNUM_Check(PiStr
017300100827     C                                          :PiDecChr
017400100827     C                                          :PiVal
017500100827     C                                          :PiNum
017600100827     C                                          :PiInt)
017700100827     C*
017800100827     C                   ENDSR
017900100827     C***
018000100826
018100100826
018200100826
018300990901     C*------------------------------------------------------------------------*
018400990901     C* *INZSR - OPERAZIONI INIZIALI
018500990901     C*------------------------------------------------------------------------*
018600990901     C     *INZSR        BEGSR
018700100827      *
018800100827      * Chiave su file TIVSS01L - Completa
018900100827     C     KEY_vss01_C   KLIST
019000100827     C                   KFLD                    vssSUN
019100100827     C                   KFLD                    vssISV
019200100827      *
019300100827      * Calcola la data corrente
019400100826     C                   eval      datcor   = %dec(%date() : *ISO)
019500001005     C*
019600990901     C                   ENDSR
019700100826     C*------------------------------------------------------------------------*
019800100826     OPRTF198   E            staLista    1
019900100826     O                       wRigaPrt            +1
