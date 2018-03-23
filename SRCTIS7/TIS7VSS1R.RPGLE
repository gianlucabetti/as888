000100100827     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000200100826     H decedit('0,') datedit(*ymd/) option(*nodebugio)
000300990901     F*------------------------------------------------------------------------*
000400990901     F* DATA BASE
000500990901     F*------------------------------------------------------------------------*
000600100907     FTIABL01L  UF   E           K DISK
000700100826     FTIVSS02L  UF   E           K DISK
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
002000100827
002100100827
002200100826     c
002300100826      *
002400100826     c     *Entry        plist
002500100826     c                   parm                    IN_ELA            1
002600100827     c                   parm                    IN_GG             3
002700100826      *
002800100826      * Verifico se lancio in aggiornamento
002900100826     c                   if        IN_ELA = 'A'
003000100826     c                   seton                                        55
003100100826     c                   else
003200100826     c                   setoff                                       55
003300100826     c                   endif
003400100827      *
003500100827      * Calcolo la data scadenza servizii prima della quale eliminare l'abilitazione
003600100827     c                   move(p)   IN_GG         wRangeBAK
003700100827     c                   eval      wDatScad = %dec(%date(datcor)-
003800100827     c                                        %days(wRangeBAK))
003900100826      *
004000100826      * Inizializzazioi contatori d wrk
004100100826     c                   z-add     *zeros        wContaUPD         6 0
004200100826     c                   z-add     *zeros        wContaTOT         6 0
004300100826      *
004400100826      * Inizializzo riga d stampa
004500100826     c                   movel     *blanks       wRigaPrt        150
004600100826      *
004700100826      * Apro il printer file
004800100826     c                   open      PRTF198
004900100826      *
005000100826     c                   if        IN_ELA = 'A'
005100100827     c                   eval      wRigaPrt='TIS7VSS1R - ' +
005200100827     c                                      '* Elaborazione in AGGIORNAMENTO *'
005300100826     c                   else
005400100827     c                   eval      wRigaPrt='TIS7VSS1R - ' +
005500100827     c                                      '* Elaborazione in SIMULAZIONE *'
005600100826     c                   endif
005700100826     c                   except    staLista
005800100827     c                   eval      wRigaPrt = 'Eliminazione scaduti prima del '+
005900100827     c                                         %char(wDatScad)
006000100827     c                   except    staLista
006100100826     c                   eval      wRigaPrt=''
006200100826     c                   except    staLista
006300100826      *
006400100827     c     *start        setll     tivss02l
006500100827     c                   read      tivss02l
006600100827     c                   dow       not %eof(tivss02l)
006700100827      *
006800100827      * Inizializzazione variabili d wrk
006900100827     c                   add       1             wContaTOT
007000100827      *
007100100827      * Verifico la data d scadenza del abilitazione corrente, se antecedente => procedo
007200100827     c                   if        vssDSC < wDatScad
007300100826      *
007400100826      * Listo utente / cliente corrente
007500100827     c                   eval      wRigaPrt = ' Utente: '   + vssSUN +
007600100827     c                                        ' Cliente: '  + vssKSU +
007700100827     c                                        ' Servizio: ' + vssISV +
007800100827     c                                        ' Scadenza: ' + %char(vssDSC)
007900100826     c                   except    staLista
008000100826     c                   add       1             wContaUPD
008100100826      *
008200100826      * Se lancio in aggiornamento => procedo effettivamente alla modifica
008300100826     c                   if        *in55
008400100827     c                   exsr      CHKSGI
008500100907     c     vssSUN        chain     tiabl01l
008600100907     c                   if        %found(tiabl01l)
008700100907     c                   delete    tiabl000
008800100907     c                   endif
008900100907     c                   delete    tivss000
009000100826     c                   endif
009100100827      *
009200100827     c                   endif
009300100826      *
009400100827      * Proseguo con la lettura sino a fine file
009500100827     c                   read      tivss02l
009600100826     c                   enddo
009700100826      *
009800100826      * Stampa totali finali
009900100826     c                   eval      wRigaPrt = *blanks
010000100826     c                   except    staLista
010100100827     c                   eval      wRigaPrt = 'TOT ABILITAZIONI ELIM: '+
010200100826     c                                         %editc(wContaUPD:'X')
010300100826     c                   except    staLista
010400100827     c                   eval      wRigaPrt = 'TOT ABILITAZIONI ELAB: '+
010500100826     c                                         %editc(wContaTOT:'X')
010600100826     c                   except    staLista
010700100826      *
010800100826      * Chiudo il printer file
010900100826     c                   close     PRTF198
011000100826      *
011100100826     c                   SETON                                        LR
011200100826     C*------------------------------------------------------------------------*
011300100827
011400100827
011500100827
011600100827     C*------------------------------------------------------------------------*
011700100827     C* CHKSGI - GESTIONE MODIFICA UTENTI SGI
011800100827     C*------------------------------------------------------------------------*
011900100827     C     CHKSGI        BEGSR
012000100827      /FREE
012100100827               if UBUTISGI_Init = 0;
012200100827               endif;
012300100827      /END-FREE
012400100827     C*
012500100827      /FREE
012600100827               pInSUN = vssSUN;
012700100827               pInKSU = vssKSU;
012800100827               pInRGR = 'S';
012900100827
013000100827               if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
013100100827               endif;
013200100827
013300100827               if pOutEsito = '0';
013400100827                  if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
013500100827                  endif;
013600100827               endif;
013700100827
013800100827      /END-FREE
013900100827     C
014000100827     C
014100100827      /FREE
014200100827               if UBUTISGI_Finalize = 0;
014300100827               endif;
014400100827      /END-FREE
014500100827     C*
014600100827     C                   ENDSR
014700100826
014800100826
014900100826
015000990901     C*------------------------------------------------------------------------*
015100990901     C* *INZSR - OPERAZIONI INIZIALI
015200990901     C*------------------------------------------------------------------------*
015300990901     C     *INZSR        BEGSR
015400100826     C*
015500100827     C* Calcola la data corrente
015600100826     C                   eval      datcor   = %dec(%date() : *ISO)
015700001005     C*
015800990901     C                   ENDSR
015900100826     C*------------------------------------------------------------------------*
016000100826     OPRTF198   E            staLista    1
016100100826     O                       wRigaPrt            +1
