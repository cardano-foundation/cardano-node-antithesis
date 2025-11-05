import type {ReactNode} from 'react';
import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<'svg'>>;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'Cardano node assets for Antithesis testing',
    Svg: require('@site/static/img/antithesis-mark.svg').default,
    description: (
      <>
        <p>
          The <strong>Antithesis</strong> project provides a set of Docker images and configurations to facilitate testing of Cardano node components in a controlled environment.
        </p>
      </>
    ),
  },
];

function Feature({title, Svg, description, imageLeft = true}: FeatureItem & {imageLeft?: boolean}) {
  return (
    <div className={clsx('col col--12')}>
      <div className="row" style={{alignItems: 'center', marginBottom: '2rem'}}>
        {imageLeft ? (
          <>
            <div className="col col--3">
              <div className="text--center">
                <Svg className={styles.featureSvg} role="img" />
              </div>
            </div>
            <div className="col col--9">
              <Heading as="h3">{title}</Heading>
              <p>{description}</p>
            </div>
          </>
        ) : (
          <>
            <div className="col col--9">
              <Heading as="h3">{title}</Heading>
              <p>{description}</p>
            </div>
            <div className="col col--3">
              <div className="text--center">
                <Svg className={styles.featureSvg} role="img" />
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} imageLeft={idx % 2 === 0} />
          ))}
        </div>
      </div>
    </section>
  );
}
