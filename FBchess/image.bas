#include once "fbgfx.bi"
#include once "jpeglib.bi"
#include once "crt.bi"

''
'' Puts a .jpg image on screen
''
''
'' dim as FB.IMAGE ptr img
'' img = imageread_jpg( exepath( ) & "/myImage.jpg", 32 )
'' put (0,0), img, pset
'' sleep
'' imagedestroy( img )

function imageread_jpg _
	( _
		byref filename as string, _
		byval bpp as integer _
	) as FB.IMAGE ptr

	dim as FILE ptr fp = fopen( filename, "rb" )
	if( fp = NULL ) then
		print "could not open image file " & filename
		return NULL
	end if

	dim jinfo as jpeg_decompress_struct
	dim jerr as jpeg_error_mgr

	jinfo.err = jpeg_std_error( @jerr )
	jpeg_create_decompress( @jinfo )

	jpeg_stdio_src( @jinfo, fp )
	jpeg_read_header( @jinfo, 1 )
	jpeg_start_decompress( @jinfo )

	select case( jinfo.out_color_space )
	case JCS_GRAYSCALE
		if( (jinfo.output_components <> 1) or (jinfo.out_color_components <> 1) ) then
			'' grayscale, but not 1 byte per pixel (will this ever happen?)
			return NULL
		end if
	case JCS_RGB
		if( (jinfo.output_components <> 3) or (jinfo.out_color_components <> 3) ) then
			'' RGB, but not 3 bytes per pixel (will this ever happen?)
			return NULL
		end if
	case JCS_YCbCr
		print "jpeg image uses YCbCr color space, not implemented"
		return NULL
	case JCS_CMYK
		print "jpeg image uses CMYK color space, not implemented"
		return NULL
	case JCS_YCCK
		print "jpeg image uses YCCK color space, not implemented"
		return NULL
	case else
		print "jpeg image uses unknown color space"
		return NULL
	end select

	dim rowbytes as integer
	rowbytes = jinfo.output_width * jinfo.output_components

	'' Allocate an array of rows (but with only 1 row, since we're going to
	'' read only one at a time)
	dim rows as JSAMPARRAY
	dim src as ubyte ptr
	rows = callocate( sizeof( ubyte ptr ) )
	rows[0] = callocate( rowbytes )
	src = rows[0]

	dim img as FB.IMAGE ptr
	img = imagecreate( jinfo.output_width, jinfo.output_height )

	dim as ubyte ptr dst = cast( ubyte ptr, img + 1 )

	while( jinfo.output_scanline < jinfo.output_height )
		jpeg_read_scanlines( @jinfo, rows, 1 )

		select case( jinfo.out_color_space )
		case JCS_GRAYSCALE
			select case( bpp )
			case 24, 32
				for i as integer = 0 to rowbytes-1
					*cptr( ulong ptr, dst ) = rgb( src[i], src[i], src[i] )
					dst += 4
				next
			case 15, 16
				for i as integer = 0 to rowbytes-1
					pset img, (i, jinfo.output_scanline-1), rgb( src[i], src[i], src[i] )
				next
			case else
				'' 8 bpp and less require a proper global palette,
				'' which contains the colors used in the image
				'for i as integer = 0 to rowbytes-1
				'	pset img, (i, jinfo.output_scanline-1), src[i]
				'next
				memcpy( dst, src, rowbytes )
				dst += img->pitch
			end select

		case JCS_RGB
			imageconvertrow( src, 24, dst, bpp, jinfo.output_width )
			dst += img->pitch
		end select
	wend

	deallocate( rows[0] )
	deallocate( rows )

	jpeg_finish_decompress( @jinfo )
	jpeg_destroy_decompress( @jinfo )
	fclose( fp )

	function = img
end function
