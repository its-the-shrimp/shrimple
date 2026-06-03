use std::{ffi::OsStr, path::Path};

use crate::inline_str;
use crate::utils::InlineStr;
use anyhow::{Context, Result, bail};
use reqwest::{Response, Url, blocking, header::HeaderMap};
use shrimple_parser::tuple::first;

macro_rules! define_mime_to_ext {
    {$( $variant_str:literal => $ext:literal ),+ $(,)?} => {
        pub fn mime_to_ext(src: &str) -> Result<InlineStr> {
            match src.split_once(['+', ';']).map_or(src, first) {
                $( $variant_str => Ok(inline_str!($ext)), )+
                _ => bail!("unknown MIME type: {src}"),
            }
        }
    };
}

define_mime_to_ext! {
    "application/atom" => ".atom",
    "application/directx" => ".x",
    "application/envoy" => ".evy",
    "application/fractals" => ".fif",
    "application/futuresplash" => ".spl",
    "application/hta" => ".hta",
    "application/internet-property-stream" => ".acx",
    "application/java-archive" => ".jar",
    "application/liquidmotion" => ".jck",
    "application/mac-binhex40" => ".hqx",
    "application/msaccess" => ".accdb",
    "application/msword" => ".doc",
    "application/octet-stream" => "",
    "application/oda" => ".oda",
    "application/oleobject" => ".ods",
    "application/olescript" => ".axs",
    "application/onenote" => ".one",
    "application/opensearchdescription" => ".osdx",
    "application/pdf" => ".pdf",
    "application/pics-rules" => ".prf",
    "application/pkcs10" => ".p10",
    "application/pkcs7-mime" => ".p7c",
    "application/pkcs7-signature" => ".p7s",
    "application/pkix-crl" => ".crl",
    "application/postscript" => ".ps",
    "application/rtf" => ".rtf",
    "application/set-payment-initiation" => ".setpay",
    "application/set-registration-initiation" => ".setreg",
    "application/streamingmedia" => ".ssm",
    "application/vnd.fdf" => ".fdf",
    "application/vnd.ms-excel" => ".xls",
    "application/vnd.ms-excel.addin.macroEnabled.12" => ".xlam",
    "application/vnd.ms-excel.sheet.binary.macroEnabled.12" => ".xlsb",
    "application/vnd.ms-excel.sheet.macroEnabled.12" => ".xlsm",
    "application/vnd.ms-excel.template.macroEnabled.12" => ".xltm",
    "application/vnd.ms-office.calx" => ".calx",
    "application/vnd.ms-officetheme" => ".thmx",
    "application/vnd.ms-pki.certstore" => ".sst",
    "application/vnd.ms-pki.pko" => ".pko",
    "application/vnd.ms-pki.seccat" => ".cat",
    "application/vnd.ms-pki.stl" => ".stl",
    "application/vnd.ms-powerpoint" => ".pot",
    "application/vnd.ms-powerpoint.addin.macroEnabled.12" => ".ppam",
    "application/vnd.ms-powerpoint.presentation.macroEnabled.12" => ".pptm",
    "application/vnd.ms-powerpoint.slide.macroEnabled.12" => ".sldm",
    "application/vnd.ms-powerpoint.slideshow.macroEnabled.12" => ".ppsm",
    "application/vnd.ms-powerpoint.template.macroEnabled.12" => ".potm",
    "application/vnd.ms-project" => ".mpp",
    "application/vnd.ms-visio.viewer" => ".vdx",
    "application/vnd.ms-word.document.macroEnabled.12" => ".docm",
    "application/vnd.ms-word.template.macroEnabled.12" => ".dotm",
    "application/vnd.ms-works" => ".wcm",
    "application/vnd.ms-xpsdocument" => ".xps",
    "application/vnd.openxmlformats-officedocument.presentationml.presentation" => ".pptx",
    "application/vnd.openxmlformats-officedocument.presentationml.slide" => ".sldx",
    "application/vnd.openxmlformats-officedocument.presentationml.slideshow" => ".ppsx",
    "application/vnd.openxmlformats-officedocument.presentationml.template" => ".potx",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" => ".xlsx",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.template" => ".xltx",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document" => ".docx",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.template" => ".dotx",
    "application/vnd.rn-realmedia" => ".rm",
    "application/vnd.visio" => ".vsd",
    "application/vnd.wap.wmlc" => ".wmlc",
    "application/vnd.wap.wmlscriptc" => ".wmlsc",
    "application/winhlp" => ".hlp",
    "application/x-bcpio" => ".bcpio",
    "application/x-cdf" => ".cdf",
    "application/x-compress" => ".z",
    "application/x-compressed" => ".tgz",
    "application/x-cpio" => ".cpio",
    "application/x-csh" => ".csh",
    "application/x-director" => ".dir",
    "application/x-dvi" => ".dvi",
    "application/x-gtar" => ".gtar",
    "application/x-gzip" => ".gz",
    "application/x-hdf" => ".hdf",
    "application/x-internet-signup" => ".ins",
    "application/x-iphone" => ".iii",
    "application/x-java-applet" => ".class",
    "application/x-javascript" => ".js",
    "application/x-latex" => ".latex",
    "application/x-miva-compiled" => ".mvc",
    "application/x-ms-application" => ".application",
    "application/x-ms-manifest" => ".manifest",
    "application/x-ms-reader" => ".lit",
    "application/x-ms-vsto" => ".vsto",
    "application/x-ms-wmd" => ".wmd",
    "application/x-ms-wmz" => ".wmz",
    "application/x-ms-xbap" => ".xbap",
    "application/x-msaccess" => ".mdb",
    "application/x-mscardfile" => ".crd",
    "application/x-msclip" => ".clp",
    "application/x-msdownload" => ".dll",
    "application/x-msmediaview" => ".m13",
    "application/x-msmetafile" => ".wmf",
    "application/x-msmoney" => ".mny",
    "application/x-mspublisher" => ".pub",
    "application/x-msschedule" => ".scd",
    "application/x-msterminal" => ".trm",
    "application/x-mswrite" => ".wri",
    "application/x-netcdf" => ".nc",
    "application/x-oleobject" => ".hhc",
    "application/x-perfmon" => ".pma",
    "application/x-pkcs12" => ".p12",
    "application/x-pkcs7-certificates" => ".p7b",
    "application/x-pkcs7-certreqresp" => ".p7r",
    "application/x-quicktimeplayer" => ".qtl",
    "application/x-sh" => ".sh",
    "application/x-shar" => ".shar",
    "application/x-shockwave-flash" => ".swf",
    "application/x-silverlight-app" => ".xap",
    "application/x-smaf" => ".mmf",
    "application/x-stuffit" => ".sit",
    "application/x-sv4cpio" => ".sv4cpio",
    "application/x-sv4crc" => ".sv4crc",
    "application/x-tar" => ".tar",
    "application/x-tcl" => ".tcl",
    "application/x-tex" => ".tex",
    "application/x-texinfo" => ".texi",
    "application/x-troff" => ".roff",
    "application/x-troff-man" => ".man",
    "application/x-troff-me" => ".me",
    "application/x-troff-ms" => ".ms",
    "application/x-ustar" => ".ustar",
    "application/x-wais-source" => ".src",
    "application/x-x509-ca-cert" => ".crt",
    "application/x-zip-compressed" => ".zip",
    "application/xaml" => ".xaml",
    "audio/aiff" => ".aiff",
    "audio/basic" => ".au",
    "audio/mid" => ".midi",
    "audio/mpeg" => ".mp3",
    "audio/wav" => ".wav",
    "audio/x-aiff" => ".aif",
    "audio/x-mpegurl" => ".m3u",
    "audio/x-ms-wax" => ".wax",
    "audio/x-ms-wma" => ".wma",
    "audio/x-pn-realaudio" => ".ra",
    "audio/x-pn-realaudio-plugin" => ".rpm",
    "audio/x-smd" => ".smd",
    "drawing/x-dwf" => ".dwf",
    "image/bmp" => ".bmp",
    "image/cis-cod" => ".cod",
    "image/gif" => ".gif",
    "image/ief" => ".ief",
    "image/jpeg" => ".jpeg",
    "image/pjpeg" => ".jfif",
    "image/png" => ".png",
    "image/svg" => ".svg",
    "image/tiff" => ".tiff",
    "image/vnd.rn-realflash" => ".rf",
    "image/vnd.wap.wbmp" => ".wbmp",
    "image/x-cmu-raster" => ".ras",
    "image/x-cmx" => ".cmx",
    "image/x-icon" => ".ico",
    "image/x-jg" => ".art",
    "image/x-portable-anymap" => ".pnm",
    "image/x-portable-bitmap" => ".pbm",
    "image/x-portable-graymap" => ".pgm",
    "image/x-portable-pixmap" => ".ppm",
    "image/x-rgb" => ".rgb",
    "image/x-xbitmap" => ".xbm",
    "image/x-xpixmap" => ".xpm",
    "image/x-xwindowdump" => ".xwd",
    "message/rfc822" => ".mhtml",
    "text/css" => ".css",
    "text/dlm" => ".dlm",
    "text/h323" => ".323",
    "text/html" => ".html",
    "text/iuls" => ".uls",
    "text/jscript" => ".jsx",
    "text/markdown" => ".md",
    "text/plain" => ".txt",
    "text/richtext" => ".rtx",
    "text/scriptlet" => ".sct",
    "text/sgml" => ".sgml",
    "text/tab-separated-values" => ".tsv",
    "text/vbscript" => ".vbs",
    "text/vnd.wap.wml" => ".wml",
    "text/vnd.wap.wmlscript" => ".wmls",
    "text/webviewhtml" => ".htt",
    "text/x-component" => ".htc",
    "text/x-hdml" => ".hdml",
    "text/x-markdown" => ".md",
    "text/x-ms-odc" => ".odc",
    "text/x-setext" => ".etx",
    "text/x-vcard" => ".vcf",
    "text/xml" => ".xml",
    "video/mpeg" => ".mpeg",
    "video/quicktime" => ".mov",
    "video/x-flv" => ".flv",
    "video/x-ivf" => ".IVF",
    "video/x-la-asf" => ".lsf",
    "video/x-ms-asf" => ".asf",
    "video/x-ms-wm" => ".wm",
    "video/x-ms-wmp" => ".wmp",
    "video/x-ms-wmv" => ".wmv",
    "video/x-ms-wmx" => ".wmx",
    "video/x-ms-wvx" => ".wvx",
    "video/x-msvideo" => ".avi",
    "video/x-sgi-movie" => ".movie",
    "x-world/x-vrml" => ".flr",
}

pub trait ResponseLike {
    fn url(&self) -> &Url;
    fn headers(&self) -> &HeaderMap;
}

impl ResponseLike for Response {
    fn url(&self) -> &Url {
        self.url()
    }

    fn headers(&self) -> &HeaderMap {
        self.headers()
    }
}

impl ResponseLike for blocking::Response {
    fn url(&self) -> &Url {
        self.url()
    }

    fn headers(&self) -> &HeaderMap {
        self.headers()
    }
}

pub fn path_extension(path: &str) -> Result<InlineStr> {
    let res = Path::new(path).extension().and_then(OsStr::to_str).unwrap_or_default();
    InlineStr::new(res).with_context(|| format!("file extension too long: {res:?}"))
}

/// Returns the file extension most suitable for the content of the HTTP response.
// TODO: don't make the HEAD request if the file extension is present
pub fn remote_file_ext(response: &impl ResponseLike) -> Result<InlineStr> {
    let path = response.url().path();
    let ext = path_extension(path)?;
    if !ext.is_empty() {
        return Ok(ext);
    }

    let content_type = response
        .headers()
        .get("Content-Type")
        .context("no Content-Type header provided")?
        .to_str()
        .context("Content-Type is not UTF-8")?;
    mime_to_ext(content_type)
}
