000100031205      *------------------------------------------------------------------------*
000200080708      * Reperisco dati OVRPRTF per printer file
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600080708     fQADSPOVR  if   e             Disk    rename(qadspovr:qadsp000)
000700080708     f                                     prefix(C_)
000800080704
000900080707     d Indx            s              3  0
001000080709     d Cmd_ovr         s            100
001100080709     d wtipmda         s                   like(吟lptipmda)
001200080709     d wtipm_a         s                   like(吟lptipm_a)
001300080708     d trulpovrds    e ds
001400080708
001500071122      *------------------------------------------------------------------------*
001600080707     c     *entry        plist
001700080708     c                   parm                    trulpovrds
001800071116      /free
001900080709       wtipmda=吟lptipmda   ;
002000080709       wtipm_a=吟lptipm_a   ;
002100080709       clear   trulpovrds   ;
002200080709       clear   cmd_ovr      ;
002300080709
002400080708       setll  *start    QADSPOVR   ;
002500080708       read             QADSPOVR   ;
002600080708
002700080708       dow   not  %eof(QADSPOVR)   ;
002800080708
002900080708       Indx=%scan('OUTQ':c_qadspovr:40) ;
003000080708         if Indx>0    ;
003100080708         助LPOOUTQ =%SUBST(c_QADSPOVR:52:10)  ;
003200080709         // Preparo la stringa della ovrprtf
003300080709         cmd_ovr='OUTQ(' + 助LPOOUTQ +')'        ;
003400080708         endif        ;
003500080708
003600080708       Indx=%scan('FORMTYPE':c_qadspovr:40) ;
003700080708         if Indx>0    ;
003800080708         助LPOFTYPE=%SUBST(C_QADSPOVR:52:10)  ;
003900080708
004000080709         if   wtipmda<>' '   ;
004100080709         Indx=%scan(wtipmda:助LPOFTYPE) ;
004200080708           IF Indx>0    ;
004300080709            %subst(吟lpoftype:Indx:2)=wtipm_a ;
004400080708           endif        ;
004500080709           endif        ;
004600080709         cmd_ovr=%trim(cmd_ovr) + ' FORMTYPE('+ 助LPOFTYPE +')' ;
004700080708         endif        ;
004800080708
004900080708       Indx=%scan('USRDTA':c_qadspovr:40)    ;
005000080708         if Indx>0    ;
005100080708         助LPOUDTA =%SUBST(c_QADSPOVR:52:10)  ;
005200080708
005300080709         if   wtipmda<>' '   ;
005400080709         Indx=%scan(wtipmda:助LPOudta ) ;
005500080708           IF Indx>0    ;
005600080709            %subst(吟lpoudta :Indx:2)=wtipm_a ;
005700080708           endif        ;
005800080709           endif        ;
005900080715         cmd_ovr=%trim(cmd_ovr) + ' USRDTA('''+ 助LPOUDTA + ''')'  ;
006000080708         endif        ;
006100090605
006200090605       Indx = %scan('SAVE' : c_qadspovr : 40);
006300090605         if Indx > *zero;
006400090605           助LPOsave = %subst(c_QADSPOVR : 52 : 5);
006500090610           cmd_ovr = %trim(cmd_ovr) + ' SAVE(' + %trimr(助LPOsave) +')';
006600090605         endif;
006700080708
006800080708       Indx=%scan('SHARE':c_qadspovr:40)    ;
006900080708         if Indx>0    ;
007000080708         助LPOSHARE=%SUBST(c_QADSPOVR:52:10)  ;
007100080709         cmd_ovr=%trim(cmd_ovr) + ' SHARE(' +助LPOSHARE + ')'  ;
007200080708         endif        ;
007300080708
007400080708       read             QADSPOVR   ;
007500080708       enddo                       ;
007600080708
007700080708       // Se sono tutti vuoti vuol dire che non ho fatto nessa OVRDBF e
007800080708       //   restituisco errore
007900080708
008000080709        吟lptipmda=wtipmda   ;
008100080709        吟lptipm_a=wtipm_a   ;
008200080709
008300080708       if 吟lposhare=*blanks   and
008400080708        吟lpooutq =*blanks   and
008500080708        吟lpoftype=*blanks   and
008600080708        吟lpoudta =*blanks       ;
008700080708        吟lpoerr='E'             ;
008800080709        else    ;
008900080709        吟lpocmo=cmd_ovr     ;
009000080708        endif                    ;
009100080708
009200080709       *inlr = *on   ;
009300080708
009400071116      /end-free
