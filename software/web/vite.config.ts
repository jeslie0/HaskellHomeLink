import { defineConfig } from 'vite'
import { exec } from "child_process"
import pluginPurgeCss from "vite-plugin-purgecss-updated-v5";
// import fs from "fs";
// import https from "https";

function purescriptPlugin() {
    return {
        name: "purescript-watch",
        handleHotUpdate: function({ file, server }) {
            if (file.endsWith(".purs")) {
                console.log("PureScript file updated")
                const command =
                    "spago bundle --bundle-type module --source-maps --outfile=pure.js --offline --minify"

                exec(command, (error, stdout, stderr) => {
                    if (error) {
                        console.error(`Error: ${error.message}`)
                        return;
                    }

                    if (stderr) {
                        console.error(`stderr: ${stderr}`)
                        return;
                    }

                    console.log(`stdout: ${stdout}`)
                    server.ws.send({
                        type: "full-reload",
                        path: "*"
                    })
                })
            }
        }
    }
}

// const httpsOptions = () => {
//     key: fs.readFileSync('/home/james/Documents/crypto/Dev_Server_Certificate_Key.pem'),
//     cert: fs.readFileSync('/home/james/Documents/crypto/Dev_Server_Certificate.crt')
// }

export default defineConfig({
    plugins: [
        purescriptPlugin(),
        pluginPurgeCss({
            variables: true
        })
    ],
    optimizeDeps: {
        include: ['apexcharts'],
    },
    build: {
        minify: 'esbuild', // Use esbuild for better performance
        rollupOptions: {
            treeshake: {
                preset: "recommended"
            },
            output: {
                manualChunks(id) {
                    if (id.includes('node_modules/apexcharts')) {
                        return 'apexcharts';
                    }
                },
            },
        },
    },

    server: {
        open: true,
        // https: httpsOptions(),
        host: true,
        proxy: {
            '/api': {
                target: 'https://localhost:8080/',
                changeOrigin: true,
                secure: false,
                // agent: new https.Agent({
                //   rejectUnauthorized: false
                // }),
                configure: (proxy, _options) => {
                    proxy.on('error', (err, _req, _res) => {
                    });
                    proxy.on('proxyReq', (proxyReq, req, _res) => {
                    });
                    proxy.on('proxyRes', (proxyRes, req, _res) => {
                    });
                },
            }
        }
    }
})
